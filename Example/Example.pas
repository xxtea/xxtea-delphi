program Example;

{$IFNDEF FPC}
{$APPTYPE CONSOLE}
{$R *.res}
{$ELSE}
{$mode objfpc}{$H+}
{$ENDIF}


uses
  {$IFNDEF FPC}System.{$ENDIF}SysUtils,
  XXTEA in '../XXTEA.pas',
  Base64 in '../Base64.pas';

var
  S, K, ED, DD: TBytes;
begin
    S := BytesOf(UTF8String('Hello World! 你好，中国！'));
    K := BytesOf('1234567890');
    ED := XXTEA.Encrypt(S, K);
    DD := XXTEA.Decrypt(ED, K);
    Writeln(Base64.Encode(ED));
    Writeln(StringOf(DD));
    Readln;
end.
