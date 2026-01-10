use crate::error::Error;
use std::collections::HashMap;

pub struct Allocator {
    segments: Vec<Segment>,
}

impl Allocator {
    pub fn new(begin: usize, end: usize) -> Self {
        Self {
            segments: vec![Segment::new(begin, end, None)],
        }
    }

    /// Allocate memory at a specific address
    pub fn allocate(&mut self, addr: usize, size: usize, name: &str) -> Result<(), Error> {
        if size == 0 {
            return Err(Error::AddressSpaceOverflow(name.to_string(), size));
        }

        let end = addr
            .checked_add(size)
            .ok_or_else(|| Error::AddressSpaceOverflow(name.to_string(), size))?;

        // Find the region(s) that overlap with the requested allocation
        for i in 0..self.segments.len() {
            let section = &self.segments[i];

            // Check if this region overlaps with our allocation
            if section.overlaps(addr, end) {
                if !section.is_free() {
                    return Err(Error::FixedAddressOverlapped(name.to_string(), addr, end));
                }

                // Replace the old region with the new split regions
                self.segments
                    .splice(i..=i, section.split(addr, size, name)?);
                return Ok(());
            }
        }

        Err(Error::AddressOutOfRange(
            name.to_string(),
            addr,
            end,
            self.segments.first().map(|r| r.begin).unwrap_or(0),
            self.segments.last().map(|r| r.end).unwrap_or(0),
        ))
    }

    /// Allocate memory within a section range [start, end)
    pub fn section(
        &mut self,
        range: (usize, usize),
        size: usize,
        name: &str,
    ) -> Result<usize, Error> {
        if size == 0 {
            return Err(Error::AddressSpaceOverflow(name.to_string(), size));
        }

        // Find the first free region within [start, end) that can fit the size
        for region in &self.segments {
            if region.is_free() {
                // Calculate the overlap between this free region and [start, end)
                let overlap_start = region.begin.max(range.0);
                let overlap_end = region.end.min(range.1);

                // Check if there's enough space in the overlap
                if overlap_start < overlap_end {
                    let available_size = overlap_end - overlap_start;
                    if available_size >= size {
                        // Allocate at the start of the overlap
                        self.allocate(overlap_start, size, name)?;
                        return Ok(overlap_start);
                    }
                }
            }
        }

        Err(Error::AddressSpaceOverflow(name.to_string(), size))
    }

    pub fn allocations(&self) -> Vec<(String, usize)> {
        self.segments
            .iter()
            .filter_map(|region| region.name.as_ref().map(|n| (n.clone(), region.begin)))
            .collect()
    }

    /// Get a map of all allocated sections with their addresses and sizes
    pub fn get_map(&self) -> HashMap<String, (usize, usize)> {
        self.segments
            .iter()
            .filter_map(|section| {
                section.name.as_ref().map(|name| {
                    let size = section.end - section.begin;
                    (name.clone(), (section.begin, size))
                })
            })
            .collect()
    }

    /// Get an iterator over sections within the specified range [begin, end)
    pub fn slice(
        &self,
        begin: usize,
        end: usize,
    ) -> impl Iterator<Item = (String, usize, usize)> + '_ {
        self.segments
            .iter()
            .filter(move |section| section.overlaps(begin, end))
            .filter_map(|section| {
                section.name.as_ref().map(|name| {
                    let size = section.end - section.begin;
                    (name.clone(), section.begin, size)
                })
            })
    }
}

/// Represent [begin, end)
#[derive(Debug, Clone)]
struct Segment {
    begin: usize,
    end: usize,
    name: Option<String>,
}

impl Segment {
    fn new(begin: usize, end: usize, name: Option<String>) -> Self {
        Self { begin, end, name }
    }

    fn is_free(&self) -> bool {
        self.name.is_none()
    }

    fn overlaps(&self, addr: usize, end: usize) -> bool {
        addr < self.end && end > self.begin
    }

    fn contains(&self, addr: usize, end: usize) -> bool {
        addr >= self.begin && end <= self.end
    }

    fn split(&self, addr: usize, size: usize, name: &str) -> Result<Vec<Self>, Error> {
        let end = addr
            .checked_add(size)
            .ok_or_else(|| Error::AddressSpaceOverflow(name.to_string(), size))?;

        // Check if the allocation fits within this section
        if !self.contains(addr, end) {
            return Err(Error::AddressOutOfRange(
                name.to_string(),
                addr,
                end,
                self.begin,
                self.end,
            ));
        }

        let mut result = Vec::new();

        // Leading section [self.begin, addr)
        if addr > self.begin {
            result.push(Segment::new(self.begin, addr, None));
        }

        // Main section [addr, end)
        result.push(Segment::new(addr, end, Some(name.to_string())));

        // Trailing section [end, self.end)
        if end < self.end {
            result.push(Segment::new(end, self.end, None));
        }

        Ok(result)
    }
}
