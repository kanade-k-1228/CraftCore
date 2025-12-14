use indexmap::IndexMap;

#[derive(Debug, Clone)]
pub enum LabelType {
    Code,
    Static(u16),
    Const(u16),
}

// (file, line_idx), LabelType, Option<PC value for code labels>
pub struct Labels {
    labels: IndexMap<String, ((String, usize), LabelType, Option<u16>)>,
}

impl Labels {
    pub fn new() -> Self {
        Labels {
            labels: IndexMap::new(),
        }
    }

    pub fn insert(
        &mut self,
        name: String,
        location: (String, usize),
        label_type: LabelType,
    ) -> Option<((String, usize), LabelType, Option<u16>)> {
        self.labels.insert(name, (location, label_type, None))
    }

    pub fn set_pc(&mut self, name: String, pc: u16) {
        if let Some(entry) = self.labels.get_mut(&name) {
            // Codeラベルの場合のみPC値を設定
            if matches!(entry.1, LabelType::Code) {
                entry.2 = Some(pc);
            }
        }
    }

    pub fn get(&self, name: &str) -> Option<&((String, usize), LabelType, Option<u16>)> {
        self.labels.get(name)
    }

    pub fn get_val(&self, name: &str) -> Option<u16> {
        self.labels
            .get(name)
            .and_then(|(_, label_type, pc_val)| match label_type {
                LabelType::Code => *pc_val,
                LabelType::Static(val) => Some(*val),
                LabelType::Const(val) => Some(*val),
            })
    }
}
