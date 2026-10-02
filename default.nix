{ emacsPackages, thread-reader-douban, gnus-thread-reader }:
emacsPackages.trivialBuild {
  pname = "nndouban";
  version = "0.1.0";
  src = ./.;
  packageRequires = [ thread-reader-douban gnus-thread-reader ];
}
