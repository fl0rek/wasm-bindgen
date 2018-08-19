use wasm_bindgen::JsValue;
use wasm_bindgen_test::*;
use wasm_bindgen::JsCast;
use js_sys::*;

#[wasm_bindgen_test]
fn new_undefined() {
    assert_eq!(Boolean::new(&JsValue::undefined()).value_of(), false);
}

#[wasm_bindgen_test]
fn new_truely() {
    assert_eq!(Boolean::new(&JsValue::from("foo")).value_of(), true);
}

#[wasm_bindgen_test]
fn boolean_inheritance() {
    let b = Boolean::new(&JsValue::from(true));
    assert!(b.is_instance_of::<Boolean>());
    assert!(b.is_instance_of::<Object>());
    let _: &Object = b.as_ref();
}
