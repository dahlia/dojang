class Dojang < Formula
  desc "Cross-platform dotfiles manager"
  homepage "https://dojang.dev/"
  url "https://github.com/dahlia/dojang/archive/refs/tags/0.2.0.tar.gz"
  sha256 "3940739f311a6a80891cfb9e45d7b84f1acea8386171fa1e60bd8edd50d5ee39"
  license "GPL-3.0-or-later"

  head do
    url "https://github.com/dahlia/dojang.git", branch: "main"
  end

  depends_on "haskell-stack" => :build

  def install
    system "stack", "setup"
    system "stack", "--jobs=#{ENV.make_jobs}", "build"#, "--flag=dojang:static"
    system "stack", "--local-bin-path=#{bin}", "install"

    completions = buildpath/"completions"
    completions.mkdir()
    (completions/"dojang.bash").write(
      Utils.safe_popen_read(bin/"dojang", "--bash-completion-script=dojang")
    )
    (completions/"dojang.zsh").write(
      Utils.safe_popen_read(bin/"dojang", "--zsh-completion-script=dojang")
    )
    (completions/"dojang.fish").write(
      Utils.safe_popen_read(bin/"dojang", "--fish-completion-script=dojang")
    )

    bash_completion.install "completions/dojang.bash"
    zsh_completion.install "completions/dojang.zsh"
    fish_completion.install "completions/dojang.fish"
  end

  test do
    system bin/"dojang", "init", "--no-interactive"
    assert_true File.exist?(testpath/"dojang.toml")
  end
end
