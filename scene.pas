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

  LMtPointsCount = 4;
  LMtRatios: array [0..LMtPointsCount + 1] of TPointRatio = (
    (0, 0.8),
    (0.20, 0.9),
    (0.23, 0.85),
    (0.26, 0.7),
    (0.29, 0.4),
    (0.30, 0)
    );

  CloudHeight = 150;
  CloudSize = 40;


type

  { TCloud }

    TCloud = record
        x, y: integer;
        nForward: TVector2D;
        Points: TPointArr;
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

implementation

function GenerateCloudPoints(): TPointArr;
var
    rbeta, rad: double;
    pcount, beta, i: integer;
begin
    pcount := random(3) + 4;
    beta := 0;
    Setlength(Result, pcount);
    for i := 0 to pcount - 1 do
    begin
        beta := beta + 360 div pcount;
        rbeta := beta * deg;
        rad :=  random(CloudSize);
        Result[i] := TPoint.Create(Round(cos(rbeta) * rad), Round(sin(rbeta) * rad));
    end;
end;

{function ResetCloud(): TCloud;
var
    cloud: TCloud;
begin
    with cloud do
    begin
        x := -2 * CloudSize;
        y := 200;
        Points := GenerateCloudPoints();
        nForward.x := 1;
        nForward.y := 0;
        Active := false;
    end;
    Result := cloud;
end;}

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

{ TCloud }

procedure TCloud.Move();
begin
    inc(X, nForward.X);
    inc(Y, nForward.Y);
end;

procedure TCloud.Reset();
begin
    x := -2 * CloudSize;
    y := CloudHeight;
    Points := GenerateCloudPoints();
    nForward.x := 1;
    nForward.y := 0;
    Active := true;
end;

end.

