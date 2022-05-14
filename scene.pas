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

  ClPCount = 9;
  CloudSize = 40;
  {CloudPoints: array [0..ClPCount - 1] of TPoint = (

  );}


type


  { TCloud }

  {  TCloud = class
    private
        FCenter: TPoint;
        FAbsPoints: TPointArr;
        FForwardV: TVector2D;
        procedure SetForwardV(AValue: TVector2D);
    public
        constructor Create(Center: TPoint; RelPoints: TPointArr);
        procedure Move();
        property ForwardV: TVector2D read FForwardV write SetForwardV;
    end;



implementation

{ TCloud }

procedure TCloud.SetForwardV(AValue: TVector2D);
begin
    if FForwardV=AValue then Exit;
    FForwardV:=AValue;
end;

constructor TCloud.Create(Center: TPoint; RelPoints: TPointArr);
var
    pcount: integer;
begin
    inherited;
end;

procedure TCloud.Move();
begin

end;
 }

  { TCloud }

  TCloud = record
    x, y: integer;
    nForward: TVector2D;
    Points: TPointArr;
  public
    procedure Move();
  end;

   function GenerateCloud(screenW, screenH: longint): TCloud;

implementation

{ TCloud }

procedure TCloud.Move();
begin

end;

function GenerateCloud(screenW, screenH: longint): TCloud;
var
    cloud: TCloud;
    pcount, alpha, beta, i: integer;
    ralpha, rbeta, rad: double;
begin
    with cloud do
    begin
        //x := random(screenW);
        //y := random(screenH);
        x := screenW div 2;
        y := screenH div 2;
        pcount := random(3) + 4;
        beta := 0;
        Setlength(Points, pcount);
        for i := 0 to pcount - 1 do
        begin
            beta := beta + 360 div pcount;
            rbeta := beta * deg;
            rad :=  random(CloudSize);
            Points[i] := TPoint.Create(Round(x + cos(rbeta) * rad), Round(y + sin(rbeta) * rad));
        end;
        alpha := random(360);
        ralpha := alpha * deg;
        nForward.x := 1;
        nForward.y := 0;
    end;
    Result := cloud;
end;

end.

