{
/**********************************************************\
|                                                          |
| XXTEA.pas                                                |
|                                                          |
| XXTEA encryption algorithm library for Delphi/FreePascal |
|                                                          |
| Encryption Algorithm Authors:                            |
|      David J. Wheeler                                    |
|      Roger M. Needham                                    |
|                                                          |
| Code Author:  Ma Bingyao <mabingyao@gmail.com>           |
| LastModified: Dec 17, 2020                               |
|                                                          |
\**********************************************************/
}

unit XXTEA;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}


interface

uses SysUtils;

function Encrypt(const Data, Key: TBytes): TBytes;
function Decrypt(const Data, Key: TBytes): TBytes;

implementation

type
  TUint32 = Cardinal;
  TUint32Array = array of TUint32;

const
  Delta: TUint32 = $9e3779b9;

function ToUint32Array(const Data: TBytes; IncludeLength: Boolean): TUint32Array;
var
  N, I, L: TUint32;
begin
  Result:= nil; // <-- If not, it all doesn't work under FPC
  L := Length(Data);
  if ((L and 3) = 0) then N := L shr 2 else N := (L shr 2) + 1;
  if (IncludeLength) then begin
    SetLength(Result, N + 1);
    Result[N] := L;
  end
  else SetLength(Result, N);
  for I := 0 to L - 1 do
  begin
    Result[I shr 2] := Result[I shr 2] or (($000000ff and Data[I]) shl ((I and 3) shl 3));
  end;
end;

function ToBytes(const Data: TUint32Array; IncludeLength: Boolean): TBytes;
var
  N, M, I: TUint32;
begin
  N := Length(Data) shl 2;
  if (IncludeLength) then
  begin
    M := Data[Length(Data) - 1];
    Dec(N, 4);
    if ((M < N - 3) or (M > N)) then
    begin
      Result := nil;
      Exit;
    end
    else N := M;
  end;
  SetLength(Result, N);
  for I := 0 to N - 1 do
    Result[I] := Byte((Data[I shr 2] shr ((I and 3) shl 3)) and $ff);
end;

function MX(Sum, Y, Z, P, E: TUint32; const K: TUint32Array): TUint32;
begin
  Result := (((Z shr 5) xor (Y shl 2)) + ((Y shr 3) xor (Z shl 4))) xor ((Sum xor Y) + (K[P and 3 xor E] xor Z));
end;

function XXTEAEncrypt(var V, K: TUint32Array): TUint32Array;
var
  N, Z, Y, Sum, E, P, Q: TUint32;
begin
  N := Length(V) - 1;
  if (N < 1) then
  begin
    Result := V;
    Exit;
  end;
  if Length(K) < 4 then SetLength(K, 4);
  Z := V[N];
  Sum := 0;
  Q := 6 + 52 div (N + 1);
  repeat
    Inc(Sum, Delta);
    E := (Sum shr 2) and 3;
    for P := 0 to N - 1 do
    begin
      Y := V[P + 1];
      inc(V[P], MX(Sum, Y, Z, P, E, K));
      Z := V[P];
    end;
    P := N;
    Y := V[0];
    inc(V[P], MX(Sum, Y, Z, P, E, K));
    Z := V[P];
    Dec(Q);
  until Q = 0;
  Result := V;
end;

function XXTEADecrypt(var V, K: TUint32Array): TUint32Array;
var
  N, Z, Y, Sum, E, P, Q: TUint32;
begin
  N := Length(V) - 1;
  if (N < 1) then
  begin
    Result := V;
    Exit;
  end;
  if Length(K) < 4 then SetLength(K, 4);
  Y := V[0];
  Q := 6 + 52 div (N + 1);
  Sum := Q * Delta;
  while (Sum <> 0) do
  begin
    E := (Sum shr 2) and 3;
    for P := N downto 1 do
    begin
      Z := V[P - 1];
      Dec(V[P], MX(Sum, Y, Z, P, E, K));
      Y := V[P];
    end;
    P := 0;
    Z := V[N];
    Dec(V[0], MX(Sum, Y, Z, P, E, K));
    Y := V[0];
    Dec(Sum, Delta);
  end;
  Result := V;
end;

function Encrypt(const Data, Key: TBytes): TBytes;
var
  V, K: TUint32Array;
begin
  if (Length(Data) = 0) then Exit;
  V := ToUint32Array(Data, True);
  K := ToUint32Array(Key, False);
  Result := ToBytes(XXTEAEncrypt(V, K), False);
end;

function Decrypt(const Data, Key: TBytes): TBytes;
var
  V, K: TUint32Array;
begin
  if (Length(Data) = 0) then exit;
  V := ToUint32Array(Data, False);
  K := ToUint32Array(Key, False);
  Result := ToBytes(XXTEADecrypt(V, K), True);
end;

end.
