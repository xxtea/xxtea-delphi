# XXTEA 加密算法的 Delphi 实现

## 简介

XXTEA 是一个快速安全的加密算法。本项目是 XXTEA 加密算法的 Delphi 实现。

它不同于原始的 XXTEA 加密算法。它是针对字节数组类型数据进行加密的，而不是针对 32 位整型数组。同样，密钥也是字节数组类型。

## 使用

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
