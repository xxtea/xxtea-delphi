# XXTEA for Delphi

<a href="https://github.com/xxtea/">
    <img src="https://avatars1.githubusercontent.com/u/6683159?v=3&s=86" alt="XXTEA logo" title="XXTEA" align="right" />
</a>

## Introduction

XXTEA is a fast and secure encryption algorithm. This is a XXTEA library for Delphi.

It is different from the original XXTEA encryption algorithm. It encrypts and decrypts byte array instead of 32bit integer array, and the key is also the byte array.

## Usage

```pascal
program Example;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  XXTEA,
  Base64;

var
  S, K, ED, DD: TBytes;
begin
    S := BytesOf(UTF8String('Hello World! 你好，中国！'));
    K := BytesOf('1234567890');
    ED := XXTEA.Encrypt(S, K);
    DD := XXTEA.Decrypt(ED, K);
    Writeln(Base64.Encode(ED));
    Readln;
end.

```
