use indexmap::IndexMap;

pub fn allocate(
    items: IndexMap<String, (u16, Option<u16>)>,
) -> Result<IndexMap<String, u16>, String> {
    let mut allocations = IndexMap::new();
    let mut occupied_ranges = Vec::new();

    // First pass: allocate items with fixed addresses
    for (name, (size, fixed_addr)) in &items {
        if let Some(addr) = fixed_addr {
            // Check for overlaps with existing allocations
            let range = (*addr, *addr + size - 1);
            for (existing_start, existing_end) in &occupied_ranges {
                if overlaps(range, (*existing_start, *existing_end)) {
                    return Err(format!(
                        "Address conflict: {} at 0x{:04X}-0x{:04X} overlaps with existing allocation",
                        name, range.0, range.1
                    ));
                }
            }

            allocations.insert(name.clone(), *addr);
            occupied_ranges.push(range);
        }
    }

    // Sort occupied ranges for efficient free space finding
    occupied_ranges.sort_by_key(|(start, _)| *start);

    // Second pass: allocate items without fixed addresses
    let mut next_free_addr = 0u16;

    for (name, (size, fixed_addr)) in &items {
        if fixed_addr.is_none() {
            // Find next available address
            let addr = find_next_free_address(next_free_addr, *size, &occupied_ranges);

            // Check for address space overflow
            if addr.saturating_add(*size) > u16::MAX {
                return Err(format!(
                    "Address space overflow: Cannot allocate {} bytes for {}",
                    size, name
                ));
            }

            allocations.insert(name.clone(), addr);
            let range = (addr, addr + size - 1);

            // Insert the new range in sorted order
            let insert_pos = occupied_ranges
                .binary_search_by_key(&addr, |(start, _)| *start)
                .unwrap_or_else(|pos| pos);
            occupied_ranges.insert(insert_pos, range);

            next_free_addr = addr + size;
        }
    }

    Ok(allocations)
}

fn overlaps(range1: (u16, u16), range2: (u16, u16)) -> bool {
    range1.0 <= range2.1 && range2.0 <= range1.1
}

fn find_next_free_address(start: u16, size: u16, occupied: &[(u16, u16)]) -> u16 {
    let mut current = start;

    for &(occupied_start, occupied_end) in occupied {
        if current + size - 1 < occupied_start {
            // Found enough space before this occupied range
            return current;
        }
        // Move past this occupied range
        current = current.max(occupied_end + 1);
    }

    current
}
