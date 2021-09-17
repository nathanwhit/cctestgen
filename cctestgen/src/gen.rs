pub(crate) mod lower;
pub(crate) mod to_rust;

use std::{cell::RefCell, convert::TryFrom};

use color_eyre::eyre::eyre;
use derive_more::{Deref, DerefMut};
use proc_macro2::TokenStream;

use crate::parser::{Descriptors, Foldable};

use self::to_rust::{CodegenCtx, ToRust};

#[allow(dead_code)]
pub(crate) fn codegen(descriptors: Descriptors, mode: Mode) -> color_eyre::Result<TokenStream> {
    let mut cloner = lower::Cloner;
    let mut mutifier = lower::Mutifier::new();
    let descriptors = descriptors.fold_with(&mut cloner).fold_with(&mut mutifier);
    let mut ctx = CodegenCtx::new();
    descriptors.to_rust(mode, &mut ctx)
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum Mode {
    Integration,
    Unit,
}

impl TryFrom<&str> for Mode {
    type Error = color_eyre::Report;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match &*value.trim().to_ascii_lowercase() {
            "integration" => Ok(Mode::Integration),
            "unit" => Ok(Mode::Unit),
            other => Err(eyre!("Invalid mode: {}", &other)),
        }
    }
}

pub(crate) trait ModeWrap: Sized {
    fn integration(self, ctx: &mut CodegenCtx) -> Integration<'_, Self>;
    fn unit(self, ctx: &mut CodegenCtx) -> Unit<'_, Self>;
}

#[derive(Deref, DerefMut)]
pub(crate) struct Integration<'ctx, T> {
    #[deref]
    #[deref_mut]
    pub(crate) inner: T,
    pub(crate) ctx: RefCell<&'ctx mut CodegenCtx>,
}

#[derive(Deref, DerefMut)]
pub(crate) struct Unit<'ctx, T> {
    #[deref]
    #[deref_mut]
    pub(crate) inner: T,
    pub(crate) ctx: RefCell<&'ctx mut CodegenCtx>,
}

impl<T> ModeWrap for T {
    fn integration(self, ctx: &mut CodegenCtx) -> Integration<'_, Self> {
        Integration {
            inner: self,
            ctx: RefCell::new(ctx),
        }
    }

    fn unit(self, ctx: &mut CodegenCtx) -> Unit<'_, Self> {
        Unit {
            inner: self,
            ctx: RefCell::new(ctx),
        }
    }
}
