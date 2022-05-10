// Copyright (c) 2022 Александр Жариков
unit drawing;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Graphics, GraphType;

    type
        TPointArr = array of TPoint;
        TPointRatio = array [0..1] of double;
        TPRatioArr = array of TPointRatio;

    procedure DrawCircle(Canvas: TCanvas; x, y, r: integer);
    procedure DrawSolidPolylineObject(Canvas: TCanvas; const Points: TPointArr; color: TColor; FloodFillStart: TPoint);

implementation

procedure DrawCircle(Canvas: TCanvas; x, y, r: integer);
begin
    Canvas.Ellipse(x - r, y - r, x + r, y + r);
end;

procedure DrawSolidPolylineObject(Canvas: TCanvas; const Points: TPointArr; color: TColor; FloodFillStart: TPoint);
begin
    Canvas.Pen.Color := color;
    Canvas.Polyline(Points);
    Canvas.Brush.Color := color;
    Canvas.FloodFill(FloodFillStart.X, FloodFillStart.Y, color, GraphType.fsBorder);
end;

end.

