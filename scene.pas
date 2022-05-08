// Copyright (c) 2022 Александр Жариков
unit scene;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Graphics;

type
    TPointArr = array of TPoint;
    TPointRatio = array [0..1] of double;
    //TPRatioArr = array of TPointRatio;

const
    Rc = 20;
    clSun = clYellow;
    clMoon = TColor($dbdbff);
    clFarMt = clLtGray;
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
        (0, 0.75),
        (0.20, 0.9),
        (0.25, 0.85),
        (0.3, 0.7),
        (0.35, 0.4),
        (0.38, 0)
    );

implementation

end.

