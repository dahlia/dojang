cask "dojang" do
  arch arm: "aarch64", intel: "x86_64"

  version "0.1.0"
  sha256 arm:   "c5e221b577c2e7734b9f65c09365fae92a8a8fcc1312a00f174d092843cbc4ee",
         intel: "e923d1dadb96a39b37747992567feaf2716b6c3a43dbc0efabf2dc1b60323342"

  url "https://github.com/dahlia/dojang/releases/download/#{version}/dojang-#{version}-macos-#{arch}.tar.xz"
  name "Dojang (binary)"
  desc "Cross-platform dotfiles manager"
  homepage "https://dojang.dev/"

  conflicts_with formula: "dojang"

  binary "dojang"

  postflight do
    if OS.send(:mac?) && Hardware::CPU.send(:arm?)
      Dir::Tmpname.create("workaround") do |tmppath|
        FileUtils.cp (staged_path/"dojang"), tmppath
        FileUtils.mv tmppath, (staged_path/"dojang")
      end
      system "/usr/bin/codesign",
             "--sign",
             "-",
             "--force",
             "--preserve-metadata=entitlements,requirements,flags,runtime",
             "#{staged_path}/dojang"
    end
    set_permissions (staged_path/"dojang"), "0755"

    completions = staged_path/"completions"
    completions.mkdir() unless completions.exist?
    (HOMEBREW_PREFIX/"etc"/"bash_completion.d"/"dojang.bash").write(
      system_command(staged_path/"dojang", args: ["--bash-completion-script=dojang"]).stdout
    )
    (HOMEBREW_PREFIX/"share"/"zsh"/"site-functions"/"dojang.zsh").write(
      system_command(staged_path/"dojang", args: ["--zsh-completion-script=dojang"]).stdout
    )
    (HOMEBREW_PREFIX/"share"/"fish"/"vendor_completions.d"/"dojang.fish").write(
      system_command(staged_path/"dojang", args: ["--fish-completion-script=dojang"]).stdout
    )
  end

  uninstall_postflight do
    (HOMEBREW_PREFIX/"etc"/"bash_completion.d"/"dojang.bash").unlink
    (HOMEBREW_PREFIX/"share"/"zsh"/"site-functions"/"dojang.zsh").unlink
    (HOMEBREW_PREFIX/"share"/"fish"/"vendor_completions.d"/"dojang.fish").unlink
  end
end
