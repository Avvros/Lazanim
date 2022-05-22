// Copyright (c) 2022 Александр Жариков
unit scene;

{$mode objfpc}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils, Graphics, drawing, calc, Types;

const
  Rc = 20;
  clSun = clYellow;
  clMoon = TColor($dbdbff);
  clFarMt = clLtGray;
  clNearMt = clGray;
  starsCount = 50;
  FarMtCount = 4;
  maxMtHeight = 300;
  // Отношения положений точек к ширине экрана и maxMtHeight
  FarMtRatios: array [0..FarMtCount + 1] of TPointRatio = (
    (0, 0.75),
    (0.15, 1),
    (0.35, 0.5),
    (0.55, 0.9),
    (0.75, 0.6),
    (1, 0.8)
    );

  LMtPointsCount = 5;
  LMtRatios: array [0..LMtPointsCount + 1] of TPointRatio = (
    (0, 0.8),
    (0.20, 0.9),
    (0.23, 0.85),
    (0.26, 0.7),
    (0.29, 0.4),
    (0.30, 0),
    (0, 0)
    );

  SpCount = 10;
  SpRatios: array [0..SpCount - 1] of TPointRatio = (
    (0.05, 0.7),
    (0.10, 0.2),
    (0.12, 0.4),
    (0.20, 0.6),
    (0.25, 0.4),
    (0.75, 0.3),
    (0.80, 0.5),
    (0.85, 0.1),
    (0.88, 0.2),
    (0.90, 0.7)
  );

  CloudHeight = 150;
  CloudSize = 40;

  BridgeH = 130;
  BridgeT = 8;


type

  { TCloud }

    TCloud = record
        x, y: integer;
        nForward: TVector2D;
        Points: TPointArr;
        FlyPhase, FlyAmpl: integer;
        TrScale: double;
        Active: boolean;
    public
        procedure Move();
        procedure Reset();
    end;

    TCloudArr = array of TCloud;

    //function ResetCloud(): TCloud;
    procedure DrawCloud(Canvas: TCanvas; cloud: TCloud);
    procedure InitClouds(var Clouds: TCloudArr);
    //function CalcFarMountains(screenW, screenH: integer): TPointArr;
    procedure MoveClouds(var Clouds: TCloudArr; screenW: integer);
    procedure trySpawnCloud(var Clouds: TCloudArr);
    procedure DrawSpruce(Canvas: TCanvas; x, y: integer; wind: integer = 0);

implementation

function GenerateCloudPoints(): TPointArr;
var
    rbeta, rad: double;
    pcount, beta, i: integer;
    res: TPointArr;
begin
    pcount := random(3) + 4;
    beta := 0;
    Setlength(res, pcount);
    for i := 0 to pcount - 1 do
    begin
        beta := beta + 360 div pcount;
        rbeta := beta * deg;
        rad :=  random(CloudSize);
        res[i] := TPoint.Create(Round(cos(rbeta) * rad), Round(sin(rbeta) * rad));
    end;
    Result := res;
end;

procedure DrawCloud(Canvas: TCanvas; cloud: TCloud);
var
    i: integer;
    buf: TPoint;
begin
    for i := 0 to Length(cloud.Points) - 1 do
    begin
        buf := cloud.Points[i];
        DrawCircle(Canvas, cloud.x + buf.X, cloud.y + buf.Y, CloudSize);
    end;
end;

{function CalcFarMountains(screenW, screenH: integer): TPointArr;
var
    bx, by, i: integer;
begin
    for i := 0 to FarMtCount + 1 do
    begin
        if i = 0 then
            bx := -1 // Чтобы убрать левую вершину за границу экрана
        else if i = FarMtCount + 1 then
            bx := screenW + 1 // Чтобы убрать правую вершину за границу экрана
        else
            bx := Trunc(FarMtRatios[i, 0] * screenW);
        by := Trunc(screenH - FarMtRatios[i, 1] * maxMtHeight);
        Result[i] := TPoint.Create(bx, by);
    end;
end;}

{function CalcNearMountains(screenW, screenH: integer): TPointArr;
var
    bx, by, i: integer;
begin
    for i := 0 to LMtPointsCount + 1 do
    begin
        if i = LMtPointsCount + 1 then
        begin
            by := screenH + 1; // Чтобы убрать правую вершину за границу экрана
        end
        else
            by := Trunc(screenH - LMtRatios[i, 1] * maxMtHeight);
        if i = 0 then
            bx := -1 // Чтобы убрать левую вершину за границу экрана
        else
            bx := Trunc(LMtRatios[i, 0] * screenW);
        Result[i] := TPoint.Create(bx, by);
        RightMountain[i] := TPoint.Create(Width - bx, by);
    end;
end; }

procedure InitClouds(var Clouds: TCloudArr);
const
    len = 3;
var
    i: integer;
begin
    SetLength(Clouds, len);
    for i := 0 to len - 1 do
    begin
        Clouds[i].Reset();
        Clouds[i].Active := false;
        Clouds[i].x := Clouds[i].x - i * 100;
    end;
end;

procedure MoveClouds(var Clouds: TCloudArr; screenW: integer);
var
    i: integer;
begin
    for i := 0 to Length(Clouds) - 1 do
    begin
        with Clouds[i] do
        begin
            if X > (screenW + 2 * CloudSize) then
                Active := false;
            if Active then Move();
        end;
    end;
end;

procedure trySpawnCloud(var Clouds: TCloudArr);
var
    num, i: integer;
begin
    num := random(150);
    if num = 0 then
    for i := 0 to Length(Clouds) - 1 do
    with Clouds[i] do
    begin
        if not Active then
        begin
            Reset;
            Break;
        end;
    end;
end;

procedure DrawSpruce(Canvas: TCanvas; x, y: integer; wind: integer);
const
    clWood = TColor($2D52A0);
begin
    with Canvas do
    begin
        Pen.Color := clWood;
        Pen.Width := 7;
        //Line(100, Height - 200, 100, Height - 150);
        Line(x, y - 50, x, y);
        Pen.Width := 1;
        Pen.Color := clGreen;
        Brush.Color := clGreen;
        //Polygon([Point(70, Height - 175), Point(130, Height - 175), Point(100, Height - 200)]);
        Polygon([Point(x - 30 + wind, y - 25), Point(x + 30 + wind, y - 25), Point(x, y - 50)]);
        //Polygon([Point(75, Height - 190), Point(125, Height - 190), Point(100, Height - 220)]);
        Polygon([Point(x - 25 + wind, y - 40), Point(x + 25 + wind, y - 40), Point(x, y - 70)]);
        //Polygon([Point(80, Height - 210), Point(120, Height - 210), Point(100, Height - 250)]);
        Polygon([Point(x - 20 + wind, y - 60), Point(x + 20 + wind, y - 60), Point(x, y - 100)]);
    end;
end;

{ TCloud }

procedure TCloud.Move();
begin
    inc(X, nForward.X);
    inc(FlyPhase);
    Y := CloudHeight + Round(sin(FlyPhase / TrScale) * FlyAmpl);
end;

procedure TCloud.Reset();
begin
    x := -2 * CloudSize;
    y := CloudHeight;
    Points := GenerateCloudPoints();
    FlyPhase := 0;
    FlyAmpl := Random(21) + 30;
    TrScale := 75 * (1 + random);
    nForward.x := 1;
    nForward.y := 0;
    Active := true;
end;

end.

