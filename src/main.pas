unit Main;

{$mode delphi}
{$WARN 5024 off : Parameter "$1" not used}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls,
  StdCtrls, OpenGLContext, FractalScene, Codebot.Graphics.Types,
  Codebot.Render.Scenes.Controller;

{ TFractalForm }

type
  TFractalForm = class(TForm)
    HelpLabel: TLabel;
    LasooButton: TSpeedButton;
    GoButton: TSpeedButton;
    InButton: TSpeedButton;
    OutButton: TSpeedButton;
    SceneControl: TOpenGLControl;
    HelpShape: TShape;
    PanButton: TSpeedButton;
    Timer: TTimer;
    XEdit: TEdit;
    XLabel: TLabel;
    YEdit: TEdit;
    YLabel: TLabel;
    ZoomEdit: TEdit;
    ZoomLabel: TLabel;
    procedure ApplicationProperties1ShowHint(var HintStr: string;
      var CanShow: Boolean; var HintInfo: THintInfo);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GoButtonClick(Sender: TObject);
    procedure InButtonClick(Sender: TObject);
    procedure OutButtonClick(Sender: TObject);
    procedure SceneControlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SceneControlMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SceneControlMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TimerTimer(Sender: TObject);
    procedure YEditKeyPress(Sender: TObject; var Key: char);
  private
    FController: TSceneController;
    FFractal: TFractalScene;
    FDragging: Boolean;
    FDrag: TRectI;
    FPanning: Boolean;
    FPan: TPointI;
  end;

var
  FractalForm: TFractalForm;

implementation

{$R *.lfm}

{ TFractalForm }

procedure TFractalForm.FormCreate(Sender: TObject);
begin
  ClientWidth := SceneControl.Width + SceneControl.Left * 2;
  ClientHeight := HelpShape.Top + HelpShape.Height + 8;
  SceneControl.Anchors := [akLeft, akTop, akRight, akBottom];
  HelpShape.Anchors := [akLeft, akRight, akBottom];
  HelpLabel.Anchors := [akLeft, akRight, akBottom];
  FController := TSceneController.Create(Self);
  FController.OpenScene(SceneControl, TFractalScene);
  FFractal := FController.Scene as TFractalScene;
  FFractal.Zoom := 1;
  FFractal.X := -0.5;
end;

procedure TFractalForm.FormShow(Sender: TObject);
begin
  OnShow := nil;
  SceneControl.SetFocus;
end;

procedure TFractalForm.GoButtonClick(Sender: TObject);
var
  X, Y, Z: Double;
begin
  Timer.Enabled := False;
  Timer.Enabled := True;
  SceneControl.SetFocus;
  if FFractal.IsTour or FDragging or FPanning then
    Exit;
  X := StrToFloatDef(XEdit.Text, -100);
  Y := StrToFloatDef(YEdit.Text, -100);
  Z := StrToFloatDef(ZoomEdit.Text, 0);
  if (X < -10) or (Y < -10) or (Z < 1.1) then
    FFractal.MoveTo(-0.5, 0, 1)
  else
    FFractal.MoveTo(X, Y, Z);
end;

procedure TFractalForm.TimerTimer(Sender: TObject);
begin
  if FFractal.IsTour or FDragging or FPanning then
    Exit;
  if XEdit.Focused or YEdit.Focused or ZoomEdit.Focused then
    Exit;
  XEdit.Text := FloatToStr(FFractal.X);
  YEdit.Text := FloatToStr(FFractal.Y);
  ZoomEdit.Text := FloatToStr(FFractal.Zoom);
end;

procedure TFractalForm.YEditKeyPress(Sender: TObject; var Key: char);
begin
  if Key = ^M then
    GoButtonClick(GoButton);
end;

procedure TFractalForm.InButtonClick(Sender: TObject);
var
  Z: Double;
begin
  Timer.Enabled := False;
  Timer.Enabled := True;
  SceneControl.SetFocus;
  if FFractal.IsTour or FDragging or FPanning then
    Exit;
  if FPanning then
    Exit;
  Z := FFractal.Zoom * 2;
  if Z < 1.1 then
    FFractal.MoveTo(-0.5, 0, 1)
  else
    FFractal.MoveTo(FFractal.X, FFractal.Y, Z);
end;

procedure TFractalForm.OutButtonClick(Sender: TObject);
var
  Z: Double;
begin
  Timer.Enabled := False;
  Timer.Enabled := True;
  SceneControl.SetFocus;
  if FFractal.IsTour or FDragging or FPanning then
    Exit;
  Z := FFractal.Zoom / 2;
  if Z < 1.1 then
    FFractal.MoveTo(-0.5, 0, 1)
  else
    FFractal.MoveTo(FFractal.X, FFractal.Y, Z);
end;

procedure TFractalForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FController.Free;
end;

procedure TFractalForm.ApplicationProperties1ShowHint(var HintStr: string;
  var CanShow: Boolean; var HintInfo: THintInfo);
begin
  Caption := HintStr;
end;

procedure TFractalForm.SceneControlMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FFractal.IsTour or FDragging or FPanning then
    Exit;
  if (ssCtrl in Shift) or PanButton.Down then
  begin
    SceneControl.Cursor := crDefault;
    FPanning := True;
    FPan.X := X;
    FPan.Y := Y;
  end
  else if Button = mbLeft then
  begin
    FDragging := True;
    SceneControl.Cursor := crCross;
    FDrag.Left := X;
    FDrag.Top := Y;
    FDrag.Width := 0;
    FDrag.Height := 0;
  end;
end;

procedure TFractalForm.SceneControlMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if FPanning or (ssCtrl in Shift) or PanButton.Down then
    SceneControl.Cursor := crSizeAll
  else if FDragging then
    SceneControl.Cursor := crCross
  else
    SceneControl.Cursor := crDefault;
  if FPanning then
  begin
    FFractal.Pan(FPan.X - X, FPan.Y - Y);
    FPan.X := X;
    FPan.Y := Y;
  end
  else if FDragging then
  begin
    FDrag.Right := X;
    FDrag.Bottom := Y;
    FFractal.Drag(FDrag);
  end;
end;

procedure TFractalForm.SceneControlMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  A, B, FX, FY, FZ: Double;
begin
  if (ssCtrl in Shift) or PanButton.Down then
    SceneControl.Cursor := crSizeAll
  else
    SceneControl.Cursor := crDefault;
  if FPanning then
  begin
    FPanning := False;
  end
  else if Button = mbLeft then
  begin
    FDragging := False;
    A := Abs(FDrag.Width);
    B := Abs(FDrag.Height);
    if (A > 0) and (B > 0) then
    begin
      A := SceneControl.Width / A;
      B := SceneControl.Height / B;
      if A > B then
        FZ := A
      else
        FZ := B;
      FX := FDrag.MidPoint.X;
      FY := FDrag.MidPoint.Y;
      FFractal.ClientToFractal(FX, FY);
      FFractal.MoveTo(FX, FY, FFractal.Zoom * FZ)
      { Optionally move directly using this code:

      FFractal.Zoom := FFractal.Zoom * FZ;
      FFractal.X := FX;
      FFractal.Y := FY;}
    end;
    FDrag := TRectI.Create;
    FFractal.Drag(FDrag);
  end
  else if Button = mbRight then
  begin
    FZ := FFractal.Zoom / 2;
    if FZ < 1.1 then
      FFractal.MoveTo(-0.5, 0, 1)
    else
      FFractal.MoveTo(FFractal.X, FFractal.Y, FZ);
  end;
end;

end.

