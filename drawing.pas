// Copyright (c) 2022 Александр Жариков
unit drawing;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Graphics;

    procedure DrawCircle(Canvas: TCanvas; x, y, r: integer);

implementation

procedure DrawCircle(Canvas: TCanvas; x, y, r: integer);
begin
    Canvas.Ellipse(x - r, y - r, x + r, y + r);
end;

end.

