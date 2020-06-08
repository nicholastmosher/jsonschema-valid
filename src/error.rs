//! Defines structured errors which may occur during validation

use std::error::Error as StdError;
use std::fmt;
use std::iter::{empty, once};

use itertools::Itertools;
use serde_json;
use serde_json::{Value, Number};
use textwrap;
use url;
use std::fmt::Formatter;

/// An error that can occur during validation.
#[derive(Debug, Clone)]
pub struct ValidationError {
    /// The kind of error that occurred.
    pub kind: ValidationErrorKind,

    /// The JSON instance fragment that had the issue.
    pub instance: Option<serde_json::Value>,

    /// The JSON schema fragment that had the issue.
    pub schema: Option<serde_json::Value>,

    /// The path to the JSON instance fragment within the entire instance document.
    pub instance_path: Vec<String>,

    /// The path to the JSON schema fragment within the entire schema.
    pub schema_path: Vec<String>,
}

/// A structured description of possible errors.
#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum ValidationErrorKind {
    /// Additional items were included but "additionalItems" was false
    AdditionalItemsNotAllowed,
    /// Additional properties were included but "additionalProperties" was false
    AdditionalPropertiesNotAllowed(Vec<String>),
    /// Instance does not contain a value it should have
    DoesNotContain,
    /// Instance value was less than the exclusive minimum from the schema
    ExclusiveMinimum(Number, Number),
    /// Instance value was greater than the exclusive maximum from the schema
    ExclusiveMaximum(Number, Number),
    /// None of the given validators succeeded for "anyOf"
    FailedAnyOf,
    /// The given validator did not fail
    FailedInversion,
    /// The value did not match the regex in the schema
    FailsPattern,
    /// Failed to resolve URL in instance document
    FailedPointer(url::Url),
    /// Failed to resolve reference
    FailedReference(String),
    /// Failed to to resolve URL
    FailedResolveUrl(String),
    /// A schema of a boolean will fail if it is false
    FalseSchema,
    /// A dependent field is not present
    InvalidDependencies,
    /// The given value does not match the format from the schema
    InvalidForFormat,
    /// A regex failed to compile
    InvalidPattern,
    /// The schema was not an object or boolean
    InvalidSchema,
    /// A URL failed to parse
    InvalidUrl(url::ParseError),
    /// The instance value did not match the type given in the schema
    InvalidType,
    /// The instance value did not match the constant in the schema
    MismatchedConst,
    /// The instance value was missing required properties
    MissingRequiredProperties(Vec<String>),
    /// The instance value was smaller than the schema's defined minimum
    Minimum(Number, Number),
    /// The instance value was greater than the schema's defined maximum
    Maximum(Number, Number),
    /// The length of the instance array was smaller than the minimum
    MinItems(usize, Number),
    /// The length of the instance array was greater than the maximum
    MaxItems(usize, Number),
    /// The length of the instance string was smaller than the minimum
    MinLength(usize, Number),
    /// The length of the instance string was greater than the maximum
    MaxLength(usize, Number),
    /// The instance object had fewer properties than the minimum
    MinProperties(usize, Number),
    /// The instance object had more properties than the maximum
    MaxProperties(usize, Number),
    /// More than one value matched the "oneOf" validator
    MoreThanOneOf,
    /// No values matched the "oneOf" validator
    NoneMatchedOneOf,
    /// The instance value did not match any of the enumerated possible values
    NotEnum,
    /// The instance number was not a multiple of the schema's multiple
    NotMultipleOf(Number, Number),
    /// The instance array contained duplicate elements
    NotUnique,
}

impl fmt::Display for ValidationErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use ValidationErrorKind::*;
        match self {
            FalseSchema => write!(f, "false schema always fails"),
            InvalidSchema => write!(f, "Invalid schema. Must be boolean or object."),
            AdditionalPropertiesNotAllowed(extras) => write!(f, "Additional properties are not allowed. Found {}.",
                                                             extras.iter().map(|x| format!("\"{}\"", x)).join(", ")),
            AdditionalItemsNotAllowed => write!(f, "Additional items are not allowed."),
            MismatchedConst => write!(f, "const doesn't match."),
            DoesNotContain => write!(f, "No items in array valid under the given schema."),
            ExclusiveMinimum(instance, schema) => write!(f, "{} <= exclusiveMinimum {}", instance, schema),
            ExclusiveMaximum(instance, schema) => write!(f, "{} >= exclusiveMaximum {}", instance, schema),
            Minimum(instance, schema) => write!(f, "{} < minimum {}", instance, schema),
            Maximum(instance, schema) => write!(f, "{} > maximum {}", instance, schema),
            NotMultipleOf(instance, schema) => write!(f, "{} not multipleOf {}", instance, schema),
            MinItems(count, min) => write!(f, "{} < minItems {}", count, min),
            MaxItems(count, max) => write!(f, "{} > maxItems {}", count, max),
            NotUnique => write!(f, "Items are not unique"),
            FailsPattern => write!(f, "Does not match pattern."),
            InvalidPattern => write!(f, "Invalid regex."),
            InvalidForFormat => write!(f, "Invalid for format."),
            MinLength(count, min) => write!(f, "{} < minLength {}", count, min),
            MaxLength(count, max) => write!(f, "{} > maxLength {}", count, max),
            InvalidDependencies => write!(f, "Invalid dependencies"),
            NotEnum => write!(f, "Value is not an enum."),
            InvalidType => write!(f, "Invalid type."),
            MissingRequiredProperties(missing) => write!(f, "Required properties {} are missing",
                                                         missing.iter().map(|x| format!("\"{}\"", x)).join(", ")),
            MinProperties(count, min) => write!(f, "{} < minProperties {}", count, min),
            MaxProperties(count, max) => write!(f, "{} > maxProperties {}", count, max),
            FailedAnyOf => write!(f, "anyOf failed"),
            NoneMatchedOneOf => write!(f, "nothing matched in oneOf"),
            MoreThanOneOf => write!(f, "More than one matched in oneOf"),
            FailedInversion => write!(f, "not"),
            FailedReference(reference) => write!(f, "Couldn't resolve reference {}", reference),
            InvalidUrl(err) => write!(f, "Invalid URL: {:?}", err),
            FailedResolveUrl(url) => write!(f, "Can't resolve url {}", url),
            FailedPointer(pointer) => write!(f, "Couldn't resolve JSON pointer {}", pointer),
        }
    }
}

impl StdError for ValidationError {}

fn path_to_string(path: &[String]) -> String {
    if path.is_empty() {
        "/".to_string()
    } else {
        "/".to_owned() + &path.iter().rev().join("/")
    }
}

impl fmt::Display for ValidationError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let msg = format!("{}", self.kind);
        writeln!(f, "{}", textwrap::fill(&msg, 78))?;

        if let Some(instance) = &self.instance {
            writeln!(
                f,
                "At instance path {}:",
                path_to_string(&self.instance_path)
            )?;

            let json_content =
                serde_json::to_string_pretty(&instance).unwrap_or_else(|_| "".to_string());
            writeln!(f, "{}", textwrap::indent(&json_content, "  "))?;
        }

        if let Some(schema) = &self.schema {
            writeln!(f, "At schema path {}:", path_to_string(&self.schema_path))?;

            let json_content =
                serde_json::to_string_pretty(&schema).unwrap_or_else(|_| "".to_string());
            writeln!(f, "{}", textwrap::indent(&json_content, "  "))?;

            if let Some(description) = schema.get("description").and_then(|x| x.as_str()) {
                writeln!(f, "Documentation for this node:")?;
                writeln!(f, "{}", textwrap::indent(&description, "  "))?;
            };
        }

        Ok(())
    }
}

impl From<url::ParseError> for ValidationError {
    fn from(err: url::ParseError) -> ValidationError {
        ValidationError::new(ValidationErrorKind::InvalidUrl(err), None, None)
    }
}

/// Stores information about a single validation error.
impl ValidationError {
    /// Create a new validation error with the given error message.
    pub fn new(kind: ValidationErrorKind, instance: Option<&Value>, schema: Option<&Value>) -> ValidationError {
        ValidationError {
            kind,
            instance: instance.cloned(),
            schema: schema.cloned(),
            instance_path: Default::default(),
            schema_path: Default::default(),
        }
    }

    /// Update the instance and schema context for the error.
    pub fn add_ctx(mut self, instance_context: String, schema_context: String) -> Self {
        self.instance_path.push(instance_context);
        self.schema_path.push(schema_context);
        self
    }

    /// Update the instance context for the error.
    pub fn instance_ctx(mut self, instance_context: String) -> Self {
        self.instance_path.push(instance_context);
        self
    }

    /// Update the schema context for the error.
    pub fn schema_ctx(mut self, schema_context: String) -> Self {
        self.schema_path.push(schema_context);
        self
    }
}

/// An `Iterator` over `ValidationError` objects. The main method by which
/// validation errors are returned to the user.
pub type ErrorIterator<'a> = Box<dyn Iterator<Item = ValidationError> + 'a>;

pub(crate) fn make_error<'a>(
    kind: ValidationErrorKind,
    instance: Option<&Value>,
    schema: Option<&Value>,
) -> ErrorIterator<'a> {
    Box::new(once(ValidationError::new(
        kind,
        instance,
        schema,
    )))
}

pub(crate) fn no_error<'a>() -> ErrorIterator<'a> {
    Box::new(empty())
}

#[cfg(test)]
mod tests {
    use crate::{schemas, Config};
    use serde_json::json;

    #[test]
    fn test_pretty_print_errors() {
        let schema = json!(
            { "properties": { "foo": { "type": "integer", "description": "HELLO" } } });
        let instance = json!({"foo": "string"});
        let cfg = Config::from_schema(&schema, Some(schemas::Draft::Draft6)).unwrap();
        let validation = cfg.validate(&instance);

        if let Err(errors) = validation {
            for error in errors {
                let formatted = format!("{}", error);
                println!("{}", formatted);
                assert!(error.instance_path == vec!("foo"));
                assert!(error.schema_path == vec!("type", "foo", "properties"));

                assert!(formatted.find("At instance path /foo").is_some());
                assert!(formatted
                    .find("At schema path /properties/foo/type")
                    .is_some());
                assert!(formatted.find("Invalid type").is_some());
                assert!(formatted.find("HELLO").is_some());
            }
        }
    }
}
