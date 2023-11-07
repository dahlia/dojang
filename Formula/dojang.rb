class Dojang < Formula
  desc "Cross-platform dotfiles manager"
  homepage "https://github.com/dahlia/dojang"
  license "GPL-3.0-or-later"
  head do
    url "https://github.com/dahlia/dojang.git", branch: "main"
    depends_on "hpack" => :build
  end

  depends_on "cabal-install" => :build
  depends_on "ghc" => :build

  def install
    system "hpack"
    system "cabal", "v2-update"
    system "cabal", "v2-install", *std_cabal_v2_args
  end

  test do
    system bin/"dojang", "init", "--no-interactive"
    assert_true File.exist?(testpath/"dojang.toml")
  end
end
