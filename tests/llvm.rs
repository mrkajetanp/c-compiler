extern crate rcc;

use rcc::*;
use serial_test::serial;

static BASIC_SAMPLE: &str = "samples/basic.c";
static UNARY_SAMPLE: &str = "samples/unary.c";
static BINARY_SAMPLE: &str = "samples/binary.c";
static DIV_SAMPLE: &str = "samples/div.c";

// ------------ basic functions ---------
//
#[test]
#[serial]
fn basic_ir() {
    let driver = Driver::new(BASIC_SAMPLE);
    driver.compile(CompileStage::IR, true);
}

#[test]
#[serial]
fn basic_codegen() {
    let driver = Driver::new(BASIC_SAMPLE);
    driver.compile(CompileStage::Codegen, true);
}

#[test]
#[serial]
fn basic_full() {
    let driver = Driver::new(BASIC_SAMPLE);
    driver.compile(CompileStage::Full, true);
    driver.clean_binary().unwrap();
}

// ------------ unary operators ---------
//
#[test]
#[serial]
fn unary_ir() {
    let driver = Driver::new(UNARY_SAMPLE);
    driver.compile(CompileStage::IR, true);
}

#[test]
#[serial]
fn unary_codegen() {
    let driver = Driver::new(UNARY_SAMPLE);
    driver.compile(CompileStage::Codegen, true);
}

#[test]
#[serial]
fn unary_full() {
    let driver = Driver::new(UNARY_SAMPLE);
    driver.compile(CompileStage::Full, true);
    driver.clean_binary().unwrap();
}

// ------------ binary operators ---------

#[test]
#[serial]
fn binary_ir() {
    let driver = Driver::new(BINARY_SAMPLE);
    driver.compile(CompileStage::IR, true);
}

#[test]
#[serial]
fn binary_codegen() {
    let driver = Driver::new(BINARY_SAMPLE);
    driver.compile(CompileStage::Codegen, true);
}

#[test]
#[serial]
fn binary_full() {
    let driver = Driver::new(BINARY_SAMPLE);
    driver.compile(CompileStage::Full, true);
    driver.clean_binary().unwrap();
}

// ------------ division operators ---------

#[test]
#[serial]
fn div_ir() {
    let driver = Driver::new(DIV_SAMPLE);
    driver.compile(CompileStage::IR, true);
}

#[test]
#[serial]
fn div_codegen() {
    let driver = Driver::new(DIV_SAMPLE);
    driver.compile(CompileStage::Codegen, true);
}

#[test]
#[serial]
fn div_full() {
    let driver = Driver::new(DIV_SAMPLE);
    driver.compile(CompileStage::Full, true);
    driver.clean_binary().unwrap();
}
