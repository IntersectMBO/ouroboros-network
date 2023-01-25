# TODO remove once https://github.com/NixOS/nixpkgs/pull/212603 is in our nixpkgs pin

{ lib, python3, fetchFromGitHub }:

python3.pkgs.buildPythonApplication {
  pname = "scriv";
  version = "1.2.0-custom-iog";

  src = fetchFromGitHub {
    owner = "input-output-hk";
    repo = "scriv";
    rev = "567a1aa3f6df77d1a531290f10a261ec6a49c75a";
    hash = "sha256-wpWDuZ3c8JJKVWPw9PHgcpneRWYjd/0z4oAIirPa0/E=";
  };

  propagatedBuildInputs = [
    python3.pkgs.attrs
    python3.pkgs.click
    python3.pkgs.click-log
    python3.pkgs.jinja2
    python3.pkgs.requests
  ] ++ lib.optionals (python3.pythonOlder "3.11") [ python3.pkgs.tomli ];

  doCheck = false;
}
