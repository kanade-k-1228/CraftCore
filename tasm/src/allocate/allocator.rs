use crate::error::LinkError;
use indexmap::IndexMap;

pub fn allocate(
    items: IndexMap<String, (usize, Option<usize>)>,
    range: (usize, usize), // (start_address, end_address_inclusive)
) -> Result<IndexMap<String, usize>, LinkError> {
    let mut result = IndexMap::new();
    let mut occupied = Vec::new(); // (begin, end)

    // Split items into "fixed address items" and "free address items"
    let (fixed, free): (Vec<_>, Vec<_>) = items.iter().partition(|(_, (_, addr))| addr.is_some());

    // 1. Allocate items with fixed addresses
    for (name, (size, addr)) in fixed {
        let addr = addr.unwrap();

        // Check if address is within the allowed range
        let (range_start, range_end) = range;
        if addr < range_start || addr + size - 1 > range_end {
            return Err(LinkError::AddressOutOfRange(
                name.to_string(),
                addr,
                addr + size - 1,
                range_start,
                range_end,
            ));
        }

        let alloc_range = (addr, addr + size - 1);
        for (begin, end) in &occupied {
            if overlaps(alloc_range, (*begin, *end)) {
                return Err(LinkError::FixedAddressOverlapped(
                    name.to_string(),
                    alloc_range.0,
                    alloc_range.1,
                ));
            }
        }
        result.insert(name.clone(), addr);
        occupied.push(alloc_range);
    }

    // 2. Sort occupied ranges
    occupied.sort_by_key(|(start, _)| *start);

    // 3. Allocate rest items
    let (range_start, range_end) = range;
    let mut next_free_addr = range_start;

    for (name, (size, _)) in free {
        // Find next available address within the allowed range
        let addr = find_next_free_address(next_free_addr, *size, &occupied);

        // Check if address is within the allowed range
        if addr < range_start || addr + size - 1 > range_end {
            return Err(LinkError::AddressOutOfRange(
                name.clone(),
                addr,
                addr + size - 1,
                range_start,
                range_end,
            ));
        }

        // Check for address space overflow
        if addr.saturating_add(*size) > usize::MAX {
            return Err(LinkError::AddressSpaceOverflow(name.clone(), *size));
        }

        result.insert(name.clone(), addr);
        let alloc_range = (addr, addr + size - 1);

        // Insert the new range in sorted order
        let insert_pos = occupied
            .binary_search_by_key(&addr, |(start, _)| *start)
            .unwrap_or_else(|pos| pos);
        occupied.insert(insert_pos, alloc_range);

        next_free_addr = addr + size;
    }

    Ok(result)
}

fn overlaps(range1: (usize, usize), range2: (usize, usize)) -> bool {
    range1.0 <= range2.1 && range2.0 <= range1.1
}

fn find_next_free_address(start: usize, size: usize, occupied: &[(usize, usize)]) -> usize {
    let mut current = start;
    for &(begin, end) in occupied {
        if current + size - 1 < begin {
            return current;
        }
        current = current.max(end + 1);
    }
    current
}
