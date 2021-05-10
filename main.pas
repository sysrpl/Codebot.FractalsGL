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
    SceneControl: TOpenGLControl;
    HelpShape: TShape;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure SceneControlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SceneControlMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SceneControlMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FController: TSceneController;
    FFractal: TFractalScene;
    FDragging: Boolean;
    FDrag: TRectI;
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

procedure TFractalForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FController.Free;
end;

procedure TFractalForm.SceneControlMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FDragging := True;
    FDrag.Left := X;
    FDrag.Top := Y;
    FDrag.Width := 0;
    FDrag.Height := 0;
  end;
end;

procedure TFractalForm.SceneControlMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if FDragging then
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
  if Button = mbLeft then
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

