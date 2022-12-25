use std::env;
use std::path::Path;

use landlock::{
    ABI, Access, AccessFs, CompatLevel, Compatible, RulesetAttr, RulesetCreated,
    RulesetCreatedAttr, path_beneath_rules,
};

use crate::rustc_info::get_cargo_home;

/// Base landlock ruleset
///
/// This allows access to various essential system locations.
pub(super) fn base_ruleset() -> RulesetCreated {
    let abi = ABI::V2;
    let access_all = AccessFs::from_all(abi);
    let access_read = AccessFs::from_read(abi);
    landlock::Ruleset::default()
        .set_compatibility(CompatLevel::SoftRequirement)
        .handle_access(AccessFs::Refer)
        .unwrap()
        .set_compatibility(CompatLevel::BestEffort)
        .handle_access(access_all)
        .unwrap()
        .create()
        .unwrap()
        .add_rules(path_beneath_rules(&["/"], access_read))
        .unwrap()
        .add_rules(path_beneath_rules(&["/tmp", "/dev/null"], access_all))
        .unwrap()
}

pub(super) fn lock_fetch() {
    let abi = ABI::V2;
    let access_all = AccessFs::from_all(abi);
    base_ruleset()
        .add_rules(path_beneath_rules([env::current_dir().unwrap().join("download")], access_all))
        .unwrap()
        .restrict_self()
        .unwrap();
}

pub(super) fn lock_build(cargo: &Path, frozen: bool) {
    let abi = ABI::V2;
    let access_all = AccessFs::from_all(abi);

    let ruleset = base_ruleset()
        .add_rules(path_beneath_rules(
            &[env::current_dir().unwrap().join("build"), env::current_dir().unwrap().join("dist")],
            access_all,
        ))
        .unwrap()
        .add_rules(path_beneath_rules(
            &[
                #[allow(deprecated)]
                &std::env::home_dir().unwrap().join(".wine"),
                Path::new("/run/user/"),
            ],
            access_all,
        ))
        .unwrap();

    let ruleset = if frozen {
        ruleset
    } else {
        ruleset
            .add_rules(path_beneath_rules(
                &[get_cargo_home(cargo).join("git"), get_cargo_home(cargo).join("registry")],
                access_all,
            ))
            .unwrap()
    };

    ruleset.restrict_self().unwrap();
}
