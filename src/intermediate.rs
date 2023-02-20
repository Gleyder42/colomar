use std::rc::Rc;

pub struct IntermediateTree(pub Vec<Root>);

pub enum Root {
    Rule(DeclaredRule),
    Enum(Enum),
    Event(Event),
}

pub struct EnumConstant {
    pub name: String
}

pub struct Enum {
    pub is_workshop: bool,
    pub constants: Vec<Rc<EnumConstant>>
}

pub enum StaticValue {
    EnumConstant(Rc<EnumConstant>)
}

pub struct Type(String);

pub struct DeclaredArgument {
    pub name: String,
    pub types: Vec<Type>,
    pub default_values: Option<StaticValue>
}

pub struct CalledArgument {
    pub declared: Rc<DeclaredArgument>,
    pub value: StaticValue
}

pub struct Event {
    pub name: String,
    pub arguments: Vec<DeclaredArgument>
}

pub struct CalledEvent {
    pub declared: Rc<Event>,
    pub arguments: CalledArgument
}

pub struct DeclaredRule {
    pub title: String,
    pub event: Rc<Event>,
    pub arguments: Vec<CalledArgument>
}