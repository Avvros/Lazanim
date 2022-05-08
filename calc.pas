// Copyright (c) 2022 Александр Жариков
unit calc;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, GraphUtil, Graphics;

const
    Epsilon = 0.001;
    Deg = PI / 180;

    function Lerp(a, b: integer; alpha: double): integer;
    function LerpColor(a, b: TColor; alpha: double): TColor;
    function InterpRatio(a, b, v: double): double;

implementation

function Lerp(a, b: integer; alpha: double): integer;
begin
    Lerp := a + Trunc((b - a) * alpha);
end;

function LerpColor(a, b: TColor; alpha: double): TColor;
var
    ha, la, sa, hb, lb, sb, hc, lc, sc: byte;
begin
    ColorToHLS(a, ha, la, sa);
    ColorToHLS(b, hb, lb, sb);
    hc := Lerp(ha, hb, alpha);
    lc := Lerp(la, lb, alpha);
    sc := Lerp(sa, sb, alpha);
    Result := HLSToColor(hc, lc, sc);
end;

function InterpRatio(a, b, v: double): double;
begin
    Result := (v - a) / (b - a);
end;

end.

