use crate::workshop::rule::*;

pub fn write_to_string(tree: &WorkshopTree) -> String {
    let mut output = String::new();
    let mut writer = WorkshopWriter::new(&mut output);

    for rule in &tree.rules {
        writer.write_def(&format!("rule(\"{}\")", rule.name));
        writer.write_opening_brace();

        let event = &rule.event;
        writer.write_def("event");
        writer.write_opening_brace();
        writer.write_statement(event.event_type.workshop_name());
        writer.write_statement(event.team.workshop_name());
        writer.write_statement(&event.slot.workshop_name());
        writer.write_closing_brace();

        let conditions = &rule.conditions;
        writer.write_space();
        writer.write_def("conditions");
        writer.write_opening_brace();
        for condition in conditions {
            writer.write_statement(&condition.workshop_name());
        }
        writer.write_closing_brace();

        let actions = &rule.actions;
        writer.write_space();
        writer.write_def("actions");
        writer.write_opening_brace();
        for action in actions {
            writer.write_statement(&action.workshop_name());
        }
        writer.write_closing_brace();

        writer.write_closing_brace();
    }

    output
}

struct WorkshopWriter<'a> {
    indention: String,
    output: &'a mut String
}

impl<'a> WorkshopWriter<'a> {
    fn new(output: &'a mut String) -> WorkshopWriter {
        WorkshopWriter { indention: String::new(), output}
    }

    fn write_def(&mut self, str: &str) {
        self.append_indention();
        self.output.push_str(str);
        self.new_line();
    }

    fn write_statement(&mut self, str: &str) {
        self.append_indention();
        self.output.push_str(str);
        self.output.push(';');
        self.new_line();
    }

    fn write_opening_brace(&mut self) {
        self.append_indention();
        self.output.push('{');
        self.new_line();
        self.indention.push_str("    ");
    }

    fn write_space(&mut self) {
        self.new_line();
    }

    fn write_closing_brace(&mut self) {
        self.indention.pop();
        self.indention.pop();
        self.indention.pop();
        self.indention.pop();
        self.append_indention();
        self.output.push('}');
        self.new_line();
    }

    fn append_indention(&mut self) {
        self.output.push_str(&self.indention);
    }

    fn new_line(&mut self) {
        self.output.push('\n');
    }
}