use schemars::schema::{InstanceType, RootSchema, Schema, SingleOrVec};
use serde_json::Value;
use std::fmt::Formatter;
use swc_common::DUMMY_SP;
use swc_ecma_codegen::text_writer::JsWriter;
use swc_ecma_codegen::{Config, Emitter};
use swc_ecma_quote::swc_common::sync::Lrc;
use swc_ecma_quote::swc_common::{FilePathMapping, SourceMap};
use swc_ecma_quote::swc_ecma_ast::{
    ArrayLit, BindingIdent, Bool, Decl, DefaultDecl, ExportDecl, ExportDefaultDecl, Expr,
    ExprOrSpread, Ident, Lit, Module, ModuleDecl, ModuleItem, Null, Number, Pat, Str, TsArrayType,
    TsInterfaceBody, TsInterfaceDecl, TsKeywordType, TsKeywordTypeKind, TsPropertySignature,
    TsType, TsTypeAnn, TsTypeElement, TsUnionOrIntersectionType, TsUnionType, VarDecl, VarDeclKind,
    VarDeclarator,
};

#[derive(Debug)]
pub enum Error {
    SchemaParse(serde_json::Error),
    CodeGen(std::io::Error),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::SchemaParse(inner) => inner.fmt(f),
            Error::CodeGen(inner) => inner.fmt(f),
        }
    }
}

impl std::error::Error for Error {}

impl From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Self {
        Self::CodeGen(err)
    }
}

impl From<serde_json::Error> for Error {
    fn from(err: serde_json::Error) -> Self {
        Self::SchemaParse(err)
    }
}

fn prop_type(value: Schema) -> Option<TsTypeAnn> {
    fn instance_type(it: InstanceType) -> TsType {
        let kind = match it {
            InstanceType::Null => TsKeywordTypeKind::TsNullKeyword,
            InstanceType::Boolean => TsKeywordTypeKind::TsBooleanKeyword,
            InstanceType::Object => TsKeywordTypeKind::TsObjectKeyword,
            InstanceType::Array => unimplemented!(),
            InstanceType::Number => TsKeywordTypeKind::TsNumberKeyword,
            InstanceType::String => TsKeywordTypeKind::TsStringKeyword,
            InstanceType::Integer => TsKeywordTypeKind::TsNumberKeyword,
        };
        TsType::TsKeywordType(TsKeywordType {
            span: DUMMY_SP,
            kind,
        })
    }

    match value {
        Schema::Bool(_) => None,
        Schema::Object(schema) => schema
            .instance_type
            .map(|i| match i {
                SingleOrVec::Single(inner) => instance_type(*inner),
                SingleOrVec::Vec(inner) => TsType::TsArrayType(TsArrayType {
                    span: DUMMY_SP,
                    elem_type: Box::new(TsType::TsUnionOrIntersectionType(
                        TsUnionOrIntersectionType::TsUnionType(TsUnionType {
                            span: DUMMY_SP,
                            types: inner
                                .into_iter()
                                .map(|ty| Box::new(instance_type(ty)))
                                .collect(),
                        }),
                    )),
                }),
            })
            .map(|ty| TsTypeAnn {
                span: DUMMY_SP,
                type_ann: Box::new(ty),
            }),
    }
}

fn inner(schema: RootSchema) -> Vec<ModuleItem> {
    let title = schema.schema.metadata.unwrap().title.unwrap();
    let object = schema.schema.object.unwrap();

    let mut items = object
        .properties
        .iter()
        .filter_map(|(name, schema)| match schema {
            Schema::Bool(_) => None,
            Schema::Object(schema) => (!schema.extensions.is_empty()).then(move || {
                schema.extensions.iter().map(move |(prop_name, value)| {
                    fn to_expr(value: Value) -> Expr {
                        let lit = match value {
                            Value::Null => Lit::Null(Null { span: DUMMY_SP }),
                            Value::Bool(value) => Lit::Bool(Bool {
                                span: DUMMY_SP,
                                value,
                            }),
                            Value::Number(value) => Lit::Num(Number {
                                span: DUMMY_SP,
                                value: value.as_f64().unwrap(),
                                raw: None,
                            }),
                            Value::String(value) => Lit::Str(Str {
                                span: DUMMY_SP,
                                value: value.into(),
                                raw: None,
                            }),
                            Value::Array(value) => {
                                return Expr::Array(ArrayLit {
                                    span: DUMMY_SP,
                                    elems: value
                                        .into_iter()
                                        .map(|v| {
                                            Some(ExprOrSpread {
                                                spread: None,
                                                expr: Box::new(to_expr(v)),
                                            })
                                        })
                                        .collect(),
                                })
                            }
                            Value::Object(_) => unimplemented!(),
                        };
                        Expr::Lit(lit)
                    }

                    ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(ExportDecl {
                        span: DUMMY_SP,
                        decl: Decl::Var(VarDecl {
                            span: DUMMY_SP,
                            kind: VarDeclKind::Const,
                            declare: false,
                            decls: vec![VarDeclarator {
                                span: DUMMY_SP,
                                name: Pat::Ident(BindingIdent {
                                    id: Ident::new(
                                        format!("{}_{}", name, prop_name).to_uppercase().into(),
                                        DUMMY_SP,
                                    ),
                                    type_ann: None,
                                }),
                                init: Some(Box::new(to_expr(value.clone()))),
                                definite: false,
                            }],
                        }),
                    }))
                })
            }),
        })
        .flatten()
        .collect::<Vec<_>>();

    let ty = TsInterfaceDecl {
        span: DUMMY_SP,
        id: Ident::new(title.into(), DUMMY_SP),
        declare: false,
        type_params: None,
        extends: vec![],
        body: TsInterfaceBody {
            span: DUMMY_SP,
            body: object
                .properties
                .into_iter()
                .map(|(name, value)| {
                    TsTypeElement::TsPropertySignature(TsPropertySignature {
                        span: DUMMY_SP,
                        readonly: true,
                        key: Box::new(Expr::Ident(Ident::new(name.into(), DUMMY_SP))),
                        computed: false,
                        optional: false,
                        init: None,
                        params: vec![],
                        type_ann: prop_type(value),
                        type_params: None,
                    })
                })
                .collect(),
        },
    };

    items.push(ModuleItem::ModuleDecl(ModuleDecl::ExportDefaultDecl(
        ExportDefaultDecl {
            span: DUMMY_SP,
            decl: DefaultDecl::TsInterfaceDecl(ty),
        },
    )));
    items
}

pub fn conv<R: std::io::Read>(schema: R) -> Result<Vec<u8>, Error> {
    let schema: RootSchema = serde_json::from_reader(schema)?;
    let m = Module {
        span: DUMMY_SP,
        body: inner(schema),
        shebang: None,
    };

    let mut code = vec![];
    let cm = Lrc::new(SourceMap::new(FilePathMapping::empty()));
    let writer = JsWriter::new(cm.clone(), "\n", &mut code, None);
    let mut emitter = Emitter {
        cfg: Config::default(),
        cm,
        comments: None,
        wr: writer,
    };

    emitter.emit_module(&m)?;
    Ok(code)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("schema.json");
        let file = std::fs::File::open(&path).unwrap();

        let ts = conv(file).unwrap();
        println!("{}", String::from_utf8(ts).unwrap());
    }
}
