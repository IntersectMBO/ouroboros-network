{ pkgs
, byron-db-converter
}:

let
  cardano-mainnet-config = pkgs.fetchurl {
    url = https://raw.githubusercontent.com/input-output-hk/cardano-node/master/mainnet-genesis.json;
    sha256 = "1ahkdhqh07096law629r1d5jf6jz795rcw6c4vpgdi5j6ysb6a2g";
  };
  cardano-mainnet-mirror = pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "cardano-mainnet-mirror";
    rev = "a31ac7534ec855b715b9a6bb6a06861ee94935d9";
    sha256 = "1z51ak4f7klz5pv2kjgaj5jv6agn2aph2n172hjssmn8x1q2bdys";
  };
  mainnet-converted = pkgs.runCommand "convert-mainnet"
    { buildInputs = [byron-db-converter ]; }
    ''
    ${byron-db-converter}/bin/byron-db-converter convert \
      --epochDir ${cardano-mainnet-mirror}/epochs \
      --dbDir $out \
      --epochSlots 21600
    '';
  validate-mainnet-immdb = pkgs.runCommand "validate-mainnet-immdb"
    { buildInputs = [ byron-db-converter ]; }
    ''
    mkdir $out
    cp -r ${mainnet-converted}/* $out
    chmod -R u+rw,g+rw,a+x $out/*
    ${byron-db-converter}/bin/byron-db-converter validate \
      --configFile ${cardano-mainnet-config} \
      --genesisHash 5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb \
      --dbDir $out \
      --onlyImmDB
    '';
in
  { inherit validate-mainnet-immdb; }
