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
  binary "completions/dojang.bash",
         target: "#{HOMEBREW_PREFIX}/etc/bash_completion.d/dojang"
  binary "completions/dojang.zsh",
         target: "#{HOMEBREW_PREFIX}/share/zsh/site-functions/_dojang"
  binary "completions/dojang.fish",
         target: "#{HOMEBREW_PREFIX}/share/fish/vendor_completions.d/dojang.fish"

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
  end
end
