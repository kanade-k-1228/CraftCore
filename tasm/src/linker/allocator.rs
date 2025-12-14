use super::structs::*;
use std::collections::HashMap;

/// Allocate addresses for all items
pub fn allocate(items: Vec<Item>) -> Result<Allocated, String> {
    let mut symbols = HashMap::new();
    let mut sections = Vec::new();

    // First pass: collect fixed address items and build initial symbol table
    let mut next_auto_addr = 0x0100; // Start auto allocation at 0x0100
    let mut code_items = Vec::new();

    for item in items {
        match item {
            Item::Static(name, size, addr) => {
                let allocated_addr = if let Some(fixed_addr) = addr {
                    fixed_addr
                } else {
                    let addr = next_auto_addr;
                    next_auto_addr += size;
                    addr
                };

                symbols.insert(name.clone(), allocated_addr);
                sections.push(Section {
                    name,
                    addr: allocated_addr,
                    data: SectionData::Data(size),
                });
            }
            Item::Const(name, size, addr) => {
                let allocated_addr = if let Some(fixed_addr) = addr {
                    fixed_addr
                } else {
                    let addr = next_auto_addr;
                    next_auto_addr += size;
                    addr
                };

                symbols.insert(name.clone(), allocated_addr);
                sections.push(Section {
                    name,
                    addr: allocated_addr,
                    data: SectionData::Data(size),
                });
            }
            Item::Code(name, code) => {
                code_items.push((name, code));
            }
        }
    }

    // Second pass: allocate code sections
    // Code typically goes into a separate region
    let mut code_addr = 0x0000; // Code starts at 0x0000

    for (name, code) in code_items {
        symbols.insert(name.clone(), code_addr);

        // Each instruction is 4 bytes (32 bits)
        let code_size = code.len();

        sections.push(Section {
            name,
            addr: code_addr,
            data: SectionData::Code(code),
        });

        code_addr += code_size;
    }

    Ok(Allocated { symbols, sections })
}
