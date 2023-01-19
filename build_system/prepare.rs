use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use super::build_sysroot::{BUILD_SYSROOT, ORIG_BUILD_SYSROOT, SYSROOT_RUSTC_VERSION, SYSROOT_SRC};
use super::path::{Dirs, RelPath};
use super::rustc_info::{get_default_sysroot, get_rustc_version};
use super::utils::{
    copy_dir_recursively, git_command, remove_file_if_exists, retry_spawn_and_wait, spawn_and_wait,
};

pub(crate) fn prepare(dirs: &Dirs, vendor: bool) {
    RelPath::DOWNLOAD.ensure_fresh(dirs);

    remove_file_if_exists(Path::new(".cargo/config.toml"));

    let mut cargo_workspaces = vec![super::build_backend::CG_CLIF.manifest_path(dirs)];

    prepare_sysroot(dirs);
    cargo_workspaces.push(super::build_sysroot::STANDARD_LIBRARY.manifest_path(dirs));
    cargo_workspaces.push(super::tests::LIBCORE_TESTS.manifest_path(dirs));

    super::abi_cafe::ABI_CAFE_REPO.fetch(dirs);
    cargo_workspaces.push(super::abi_cafe::ABI_CAFE.manifest_path(dirs));
    super::tests::RAND_REPO.fetch(dirs);
    cargo_workspaces.push(super::tests::RAND.manifest_path(dirs));
    super::tests::REGEX_REPO.fetch(dirs);
    cargo_workspaces.push(super::tests::REGEX.manifest_path(dirs));
    super::tests::PORTABLE_SIMD_REPO.fetch(dirs);
    cargo_workspaces.push(super::tests::PORTABLE_SIMD.manifest_path(dirs));
    super::bench::SIMPLE_RAYTRACER_REPO.fetch(dirs);
    cargo_workspaces.push(super::bench::SIMPLE_RAYTRACER.manifest_path(dirs));

    if vendor {
        let mut vendor_cmd = Command::new("cargo");

        vendor_cmd.arg("vendor").arg("--manifest-path").arg(&cargo_workspaces[0]);

        for workspace in cargo_workspaces.iter().skip(1) {
            vendor_cmd.arg("--sync").arg(workspace);
        }

        vendor_cmd.arg("download/vendor");

        let vendor_output = vendor_cmd.stderr(Stdio::inherit()).output().unwrap();
        assert!(vendor_output.status.success());
        let replacement_config = String::from_utf8(vendor_output.stdout).unwrap();

        std::fs::create_dir_all(".cargo").unwrap();
        std::fs::write(".cargo/config.toml", replacement_config).unwrap();
    } else {
        for workspace in &cargo_workspaces {
            let mut fetch_cmd = Command::new("cargo");
            fetch_cmd.arg("fetch").arg("--manifest-path").arg(workspace);
            spawn_and_wait(fetch_cmd)
        }
    }
}

fn prepare_sysroot(dirs: &Dirs) {
    let sysroot_src_orig = get_default_sysroot(Path::new("rustc")).join("lib/rustlib/src/rust");
    assert!(sysroot_src_orig.exists());

    eprintln!("[COPY] sysroot src");

    // FIXME ensure builds error out or update the copy if any of the files copied here change
    BUILD_SYSROOT.ensure_fresh(dirs);
    copy_dir_recursively(&ORIG_BUILD_SYSROOT.to_path(dirs), &BUILD_SYSROOT.to_path(dirs));

    fs::create_dir_all(SYSROOT_SRC.to_path(dirs).join("library")).unwrap();
    copy_dir_recursively(
        &sysroot_src_orig.join("library"),
        &SYSROOT_SRC.to_path(dirs).join("library"),
    );

    let rustc_version = get_rustc_version(Path::new("rustc"));
    fs::write(SYSROOT_RUSTC_VERSION.to_path(dirs), &rustc_version).unwrap();

    eprintln!("[GIT] init");
    init_git_repo(&SYSROOT_SRC.to_path(dirs));

    apply_patches(dirs, "sysroot", &SYSROOT_SRC.to_path(dirs));
}

pub(crate) struct GitRepo {
    url: GitRepoUrl,
    rev: &'static str,
    patch_name: &'static str,
}

enum GitRepoUrl {
    Github { user: &'static str, repo: &'static str },
}

impl GitRepo {
    pub(crate) const fn github(
        user: &'static str,
        repo: &'static str,
        rev: &'static str,
        patch_name: &'static str,
    ) -> GitRepo {
        GitRepo { url: GitRepoUrl::Github { user, repo }, rev, patch_name }
    }

    pub(crate) const fn source_dir(&self) -> RelPath {
        match self.url {
            GitRepoUrl::Github { user: _, repo } => RelPath::DOWNLOAD.join(repo),
        }
    }

    fn fetch(&self, dirs: &Dirs) {
        match self.url {
            GitRepoUrl::Github { user, repo } => {
                clone_repo_shallow_github(
                    dirs,
                    &self.source_dir().to_path(dirs),
                    user,
                    repo,
                    self.rev,
                );
            }
        }
        apply_patches(dirs, self.patch_name, &self.source_dir().to_path(dirs));
    }
}

#[allow(dead_code)]
fn clone_repo(download_dir: &Path, repo: &str, rev: &str) {
    eprintln!("[CLONE] {}", repo);
    // Ignore exit code as the repo may already have been checked out
    git_command(None, "clone").arg(repo).arg(download_dir).spawn().unwrap().wait().unwrap();

    let mut clean_cmd = git_command(download_dir, "checkout");
    clean_cmd.arg("--").arg(".");
    spawn_and_wait(clean_cmd);

    let mut checkout_cmd = git_command(download_dir, "checkout");
    checkout_cmd.arg("-q").arg(rev);
    spawn_and_wait(checkout_cmd);
}

fn clone_repo_shallow_github(dirs: &Dirs, download_dir: &Path, user: &str, repo: &str, rev: &str) {
    if cfg!(windows) {
        // Older windows doesn't have tar or curl by default. Fall back to using git.
        clone_repo(download_dir, &format!("https://github.com/{}/{}.git", user, repo), rev);
        return;
    }

    let archive_url = format!("https://github.com/{}/{}/archive/{}.tar.gz", user, repo, rev);
    let archive_file = RelPath::DOWNLOAD.to_path(dirs).join(format!("{}.tar.gz", rev));
    let archive_dir = RelPath::DOWNLOAD.to_path(dirs).join(format!("{}-{}", repo, rev));

    eprintln!("[DOWNLOAD] {}/{} from {}", user, repo, archive_url);

    // Remove previous results if they exists
    let _ = std::fs::remove_file(&archive_file);
    let _ = std::fs::remove_dir_all(&archive_dir);
    let _ = std::fs::remove_dir_all(&download_dir);

    // Download zip archive
    let mut download_cmd = Command::new("curl");
    download_cmd
        .arg("--max-time")
        .arg("600")
        .arg("-y")
        .arg("30")
        .arg("-Y")
        .arg("10")
        .arg("--connect-timeout")
        .arg("30")
        .arg("--continue-at")
        .arg("-")
        .arg("--location")
        .arg("--output")
        .arg(&archive_file)
        .arg(archive_url);
    retry_spawn_and_wait(5, download_cmd);

    // Unpack tar archive
    let mut unpack_cmd = Command::new("tar");
    unpack_cmd.arg("xf").arg(&archive_file).current_dir(RelPath::DOWNLOAD.to_path(dirs));
    spawn_and_wait(unpack_cmd);

    // Rename unpacked dir to the expected name
    std::fs::rename(archive_dir, &download_dir).unwrap();

    init_git_repo(&download_dir);

    // Cleanup
    std::fs::remove_file(archive_file).unwrap();
}

fn init_git_repo(repo_dir: &Path) {
    let mut git_init_cmd = git_command(repo_dir, "init");
    git_init_cmd.arg("-q");
    spawn_and_wait(git_init_cmd);

    let mut git_add_cmd = git_command(repo_dir, "add");
    git_add_cmd.arg(".");
    spawn_and_wait(git_add_cmd);

    let mut git_commit_cmd = git_command(repo_dir, "commit");
    git_commit_cmd.arg("-m").arg("Initial commit").arg("-q");
    spawn_and_wait(git_commit_cmd);
}

fn get_patches(dirs: &Dirs, crate_name: &str) -> Vec<PathBuf> {
    let mut patches: Vec<_> = fs::read_dir(RelPath::PATCHES.to_path(dirs))
        .unwrap()
        .map(|entry| entry.unwrap().path())
        .filter(|path| path.extension() == Some(OsStr::new("patch")))
        .filter(|path| {
            path.file_name()
                .unwrap()
                .to_str()
                .unwrap()
                .split_once("-")
                .unwrap()
                .1
                .starts_with(crate_name)
        })
        .collect();
    patches.sort();
    patches
}

fn apply_patches(dirs: &Dirs, crate_name: &str, target_dir: &Path) {
    if crate_name == "<none>" {
        return;
    }

    for patch in get_patches(dirs, crate_name) {
        eprintln!(
            "[PATCH] {:?} <- {:?}",
            target_dir.file_name().unwrap(),
            patch.file_name().unwrap()
        );
        let mut apply_patch_cmd = git_command(target_dir, "am");
        apply_patch_cmd.arg(patch).arg("-q");
        spawn_and_wait(apply_patch_cmd);
    }
}
