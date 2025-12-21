use crate::error::LinkError;

#[derive(Debug, Clone)]
struct Section {
    begin: usize,
    end: usize,
    name: Option<String>, // None means free
}

impl Section {
    fn new(begin: usize, end: usize, name: Option<String>) -> Self {
        Self { begin, end, name }
    }

    fn is_free(&self) -> bool {
        self.name.is_none()
    }

    fn overlaps(&self, addr: usize, end: usize) -> bool {
        addr <= self.end && end >= self.begin
    }

    fn contains(&self, addr: usize, end: usize) -> bool {
        addr >= self.begin && end <= self.end
    }

    fn split(&self, addr: usize, size: usize, name: &str) -> Result<Vec<Self>, LinkError> {
        let end = addr
            .checked_add(size - 1)
            .ok_or_else(|| LinkError::AddressSpaceOverflow(name.to_string(), size))?;

        // Check if the allocation fits within this section
        if !self.contains(addr, end) {
            return Err(LinkError::AddressOutOfRange(
                name.to_string(),
                addr,
                end,
                self.begin,
                self.end,
            ));
        }

        let mut result = Vec::new();

        // Leading section
        if addr > self.begin {
            result.push(Section::new(self.begin, addr - 1, None));
        }

        // Main section
        result.push(Section::new(addr, end, Some(name.to_string())));

        // Tailing section
        if end < self.end {
            result.push(Section::new(end + 1, self.end, None));
        }

        Ok(result)
    }
}

pub struct Allocator {
    map: Vec<Section>,
}

impl Allocator {
    pub fn new(start: usize, end: usize) -> Self {
        Self {
            map: vec![Section::new(start, end, None)],
        }
    }

    /// Allocate memory at a specific address
    pub fn allocate(&mut self, addr: usize, size: usize, name: &str) -> Result<(), LinkError> {
        if size == 0 {
            return Err(LinkError::AddressSpaceOverflow(name.to_string(), size));
        }

        let end = addr
            .checked_add(size - 1)
            .ok_or_else(|| LinkError::AddressSpaceOverflow(name.to_string(), size))?;

        // Find the region(s) that overlap with the requested allocation
        for i in 0..self.map.len() {
            let section = &self.map[i];

            // Check if this region overlaps with our allocation
            if section.overlaps(addr, end) {
                if !section.is_free() {
                    return Err(LinkError::FixedAddressOverlapped(
                        name.to_string(),
                        addr,
                        end,
                    ));
                }

                // Replace the old region with the new split regions
                self.map.splice(i..=i, section.split(addr, size, name)?);
                return Ok(());
            }
        }

        Err(LinkError::AddressOutOfRange(
            name.to_string(),
            addr,
            end,
            self.map.first().map(|r| r.begin).unwrap_or(0),
            self.map.last().map(|r| r.end).unwrap_or(0),
        ))
    }

    /// Allocate memory within a section range [start, end]
    pub fn section(
        &mut self,
        start: usize,
        end: usize,
        size: usize,
        name: &str,
    ) -> Result<usize, LinkError> {
        if size == 0 {
            return Err(LinkError::AddressSpaceOverflow(name.to_string(), size));
        }

        // Find the first free region within [start, end] that can fit the size
        for region in &self.map {
            if region.is_free() {
                // Calculate the overlap between this free region and [start, end]
                let overlap_start = region.begin.max(start);
                let overlap_end = region.end.min(end);

                // Check if there's enough space in the overlap
                if overlap_start <= overlap_end {
                    let available_size = overlap_end - overlap_start + 1;
                    if available_size >= size {
                        // Allocate at the start of the overlap
                        self.allocate(overlap_start, size, name)?;
                        return Ok(overlap_start);
                    }
                }
            }
        }

        Err(LinkError::AddressSpaceOverflow(name.to_string(), size))
    }

    pub fn allocations(&self) -> Vec<(String, usize)> {
        self.map
            .iter()
            .filter_map(|region| region.name.as_ref().map(|n| (n.clone(), region.begin)))
            .collect()
    }
}
