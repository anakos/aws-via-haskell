{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, amazonka, amazonka-core
      , amazonka-dynamodb, amazonka-ec2, amazonka-iam, amazonka-lambda
      , amazonka-rds, amazonka-s3, amazonka-sdb, amazonka-sqs
      , amazonka-sts, base, bytestring, conduit-extra, directory
      , filepath, lens, protolude, resourcet, stdenv, text, text-format
      , time, unordered-containers, zip-archive
      }:
      mkDerivation {
        pname = "aws-via-haskell";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          amazonka amazonka-core base bytestring lens protolude resourcet
          text
        ];
        executableHaskellDepends = [
          aeson amazonka-dynamodb amazonka-ec2 amazonka-iam amazonka-lambda
          amazonka-rds amazonka-s3 amazonka-sdb amazonka-sqs amazonka-sts
          base bytestring conduit-extra directory filepath lens protolude
          resourcet text text-format time unordered-containers zip-archive
        ];
        homepage = "https://github.com/rcook/aws-via-haskell#readme";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
