// Copyright (c) 2022 Александр Жариков
unit main;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ActnList, Math,
    StdCtrls;

type

    { TMainForm }

    TMainForm = class(TForm)
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        Stop: TAction;
        ActionList1: TActionList;
        Timer1: TTimer;
        procedure FormCreate(Sender: TObject);
        procedure FormPaint(Sender: TObject);
        procedure DrawPic();
        procedure DrawAtmosphere();
        procedure DrawBackground();
        procedure DrawForeground();
        procedure CalcFarMountains();
        procedure CalcNearMountains();
        procedure DrawSpruces();
        procedure DrawBridge();
        procedure DrawTrain();
        procedure FormResize(Sender: TObject);
        procedure StopExecute(Sender: TObject);
        procedure Timer1Timer(Sender: TObject);
        procedure InvalidateMeasures();
    private

    public

    end;

var
    MainForm: TMainForm;

implementation
uses drawing, calc, scene;

var
    sun_x, sun_y, sun_y0, sun_x0, px, py, trainSg: integer;
    alpha, speed, spdx, trainDx: double;
    clAtmo: TColor;
    FarMountains, LeftMountain, RightMountain: TPointArr;
    Stars: TPRatioArr;
   // ExCloud: TCloud;
    Clouds: array of TCloud;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.DrawAtmosphere();
var
    i: integer;
begin
    Canvas.Brush.Color := clAtmo;
    Canvas.Clear;
    for i := 0 to starsCount - 1 do
    begin
        Canvas.Pixels[Trunc(Stars[i, 0] * Width), Trunc(Stars[i, 1] * Height)] := clWhite;
    end;
end;

procedure TMainForm.DrawBackground();
var
    ffs: TPoint; // FloodFill start
begin
    ffs := TPoint.Create(10, Height - 10);
    DrawSolidPolylineObject(Canvas, FarMountains, clFarMt, ffs);
end;

procedure TMainForm.DrawForeground();
const
    clLtLtGray = TColor($E0E0E0);
var
    ffs: TPoint;
    moon_x, moon_y, i: integer;
begin
    Canvas.Brush.Color := clSun;
    DrawCircle(Canvas, sun_x, sun_y, Rc);
    moon_x := Width - sun_x;
    moon_y := 2 * Height - sun_y;
    Canvas.Brush.Color := clMoon;
    DrawCircle(Canvas, moon_x, moon_y, Rc);
    DrawBridge();
    DrawTrain();
    ffs := TPoint.Create(10, Height - 10);
    DrawSolidPolylineObject(Canvas, LeftMountain, clNearMt, ffs);
    ffs := TPoint.Create(Width - 10, Height - 10);
    DrawSolidPolylineObject(Canvas, RightMountain, clNearMt, ffs);
    Canvas.Brush.Color := clLtLtGray;
    Canvas.Pen.Color := clLtLtGray;
    for i := 0 to Length(Clouds) - 1 do
        DrawCloud(Canvas, Clouds[i]);
    //DrawSpruce(Canvas, 100, Height - 150);
    DrawSpruces();
end;

procedure SetTime(time: double);
begin
    clAtmo := LerpColor(clBlack, clWhite, time);
end;

procedure TMainForm.InvalidateMeasures();
var
    R: integer;
begin
    py := Height;
    px := Width div 2;
    R := Height - Rc;
    sun_x0 := px - R;
    sun_y0 := Height;
    CalcFarMountains();
    CalcNearMountains();
end;

procedure GenerateStars();
var
    i: integer;
begin
    SetLength(Stars, starsCount);
    for i := 0 to starsCount - 1 do
    begin
        Stars[i, 0] := random;
        Stars[i, 1] := random;
    end;
end;

procedure TMainForm.DrawSpruces();
const
    dl = 5;
var
    i: integer;
begin
    for i := 0 to SpCount - 1 do
    begin
        DrawSpruce(Canvas,
                    Round(SpRatios[i, 0] * Width),
                    Round(Height - SpRatios[i, 1] * MaxMtHeight),
                    Round(dl * spdx));
    end;
end;

procedure TMainForm.DrawBridge();
const
    clBridge = TColor($87B8DE);
    //clBridge = TColor($3F85CD);
var
    i, p: integer;
begin
    Canvas.Pen.Color := clBridge;
    Canvas.Pen.Width := BridgeT;
    Canvas.Line(Width div 4, Height - BridgeH, Width div 4 * 3, Height - BridgeH);
    for i := 1 to 4 do
    begin
        p := Width div 4 + Width div 10 * i;
        Canvas.Line(p, Height, p, Height - BridgeH);
    end;
end;

procedure TMainForm.DrawTrain();
const
    wr = 10; // Wheel radius
var
    swr, wh, tl, tr, tt, nt, nb, px, tx: integer;
begin
    swr := wr * trainSg;
    tx := Round(Width div 2 * (1 + trainDx)); // Train X
    Canvas.Pen.Width := 1;
    Canvas.Pen.Color := clMedGray;
    Canvas.Brush.Color := clMedGray;
    wh := Height - BridgeH - BridgeT div 2 - wr; // Wheel height
    DrawCircle(Canvas, tx, wh, wr);
    DrawCircle(Canvas, tx + 2 * swr, wh, wr);
    DrawCircle(Canvas, tx + 8 * swr, wh, wr);
    tl := tx - swr; // Train left
    tr := tx + 9 * swr; //Train right
    tt := wh - 3 * wr; // Train top
    Canvas.Rectangle(tl, tt, tr, wh); // Корпус (от левого верхнего угла, до правого нижнего)
    Canvas.Rectangle(tl, tt, tl + 3 * swr, tt - 2 * wr); // Верхняя часть кабины
    nt := wh - wr; // Nose top
    //ns := wr div 2 * 3; // Nose side
    nb := wh + wr div 2; // Nose bottom
    Canvas.Polygon([Point(tr, nt),
                    Point(tr, nb),
                    Point(tr + (nb - nt) * trainSg, nb)]); // Нос
    px := tr - 2 * swr; // Pipe X
    Canvas.Polygon([Point(px - 3, tt),
                    Point(px - wr, tt - 2 * wr),
                    Point(px - wr, tt - 5 * wr div 2),
                    Point(px + wr, tt - 5 * wr div 2),
                    Point(px + wr, tt - 2 * wr),
                    Point(px + 3, tt)]);
end;

procedure TMainForm.CalcFarMountains();
var
    bx, by, i: integer;
begin
    for i := 0 to FarMtCount + 1 do
    begin
        if i = 0 then
            bx := -1 // Чтобы убрать левую вершину за границу экрана
        else if i = FarMtCount + 1 then
            bx := Width + 1 // Чтобы убрать правую вершину за границу экрана
        else
            bx := Trunc(FarMtRatios[i, 0] * Width);
        by := Trunc(Height - FarMtRatios[i, 1] * maxMtHeight);
        FarMountains[i] := TPoint.Create(bx, by);
    end;
end;

procedure TMainForm.CalcNearMountains();
var
    bx, by, i: integer;
begin
    for i := 0 to LMtPointsCount + 1 do
    begin
        if i = LMtPointsCount + 1 then
        begin
            by := Height + 1; // Чтобы убрать правую вершину за границу экрана
        end
        else
            by := Trunc(Height - LMtRatios[i, 1] * maxMtHeight);
        if i = 0 then
            bx := -1 // Чтобы убрать левую вершину за границу экрана
        else
            bx := Trunc(LMtRatios[i, 0] * Width);
        LeftMountain[i] := TPoint.Create(bx, by);
        RightMountain[i] := TPoint.Create(Width - bx, by);
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
    Randomize;
    GenerateStars();
    speed := 0.5;
    Setlength(FarMountains, FarMtCount + 2);
    Setlength(LeftMountain, LMtPointsCount + 2);
    Setlength(RightMountain, LMtPointsCount + 2);
    InvalidateMeasures();
    SetTime(1);
    alpha := 0;
    sun_x := sun_x0;
    sun_y := sun_y0;
    InitClouds(Clouds);
end;

procedure TMainForm.DrawPic();
begin
    DrawAtmosphere();
    DrawBackground();
    DrawForeground();
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
    InvalidateMeasures();
end;

procedure TMainForm.StopExecute(Sender: TObject);
begin
    Timer1.Enabled := not Timer1.Enabled;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
var
    vx, vy: integer;
    nvx, nvy, ralpha, sina, cosa: Double;
begin
    alpha := alpha + 1 * speed;
    if (alpha > 360) then alpha := alpha - 360;
    vx := sun_x - px;
    vy := sun_y - py;
    ralpha := Deg * alpha;
    sina := sin(ralpha);
    cosa := cos(ralpha);
    nvx := vx * cosa - vy * sina;
    nvy := vx * sina + vy * cosa;
    sun_x := Trunc(nvx) + px;
    sun_y := Trunc(nvy) + py;
    SetTime((sina + 1) / 2);
    MoveClouds(Clouds, Width);
    {Label1.Caption := IntToStr(Clouds[0].x);
    Label2.Caption := IntToStr(Clouds[1].x);
    Label3.Caption := IntToStr(Clouds[2].x);}
    trySpawnCloud(Clouds);
    spdx := sin(ralpha * 2);
    trainSg := sign(cosa);
    trainDx := sina;
    Repaint;  // Заменил DrawPic на Repaint, т.к иначе картинка очень сильно мерцает
    sun_x := sun_x0;
    sun_y := sun_y0;
end;

procedure TMainForm.FormPaint(Sender: TObject);
begin
    DrawPic();
end;

end.

