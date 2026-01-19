cask "dojang" do
  arch arm: "aarch64", intel: "x86_64"

  version "0.2.0"
  sha256 arm:   "20c6c330aa480d2d54949a3379c82bd0c64865039688f37e406abc5620e294d3",
         intel: "779ce556294193a68198ab49282d05808256908dc1afcdf8e75b555f26c757b2"

  url "https://github.com/dahlia/dojang/releases/download/#{version}/dojang-#{version}-macos-#{arch}.tar.xz"
  name "Dojang (binary)"
  desc "Cross-platform dotfiles manager"
  homepage "https://dojang.dev/"

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
