cask "dojang" do
  arch arm: "aarch64", intel: "x86_64"

  version "0.2.1"
  sha256 arm:   "5ec6d9c35def97cb75440cb9b6de08e884f275c72f1b52eb930c74ac7b894ba0",
         intel: "c89e475712ab4f66569299355febf5e18e510aab7a63365e8d58017feb1041de"

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
