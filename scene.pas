// Copyright (c) 2022 Александр Жариков
unit scene;

{$mode objfpc}{$H+}
{$modeSwitch advancedRecords}

interface

uses
    Classes, SysUtils, Graphics, drawing, calc;

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
    FarMtRatios : array [0..FarMtCount + 1] of TPointRatio = (
        (0, 0.75),
        (0.15, 1),
        (0.35, 0.5),
        (0.55, 0.9),
        (0.75, 0.6),
        (1, 0.8)
    );

    LMtPointsCount = 4;
    LMtRatios : array [0..LMtPointsCount + 1] of TPointRatio = (
        (0, 0.8),
        (0.20, 0.9),
        (0.23, 0.85),
        (0.26, 0.7),
        (0.29, 0.4),
        (0.30, 0)
    );

type


    { TCloud }

    TCloud = record
        x, y: integer;
        nForward: TVector2D;
        Points: TPointArr;
    public
        procedure Move();
    end;



implementation

{ TCloud }

procedure TCloud.Move();
begin

end;

end.

