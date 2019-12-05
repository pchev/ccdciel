unit pu_keyboard;
{ simple on-screen keyboard}
{ interface via three strings:}
{ keyboard_text}
{ keyboard_caption}
{ keyboard_question}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { Tkeyboard1 }

  Tkeyboard1 = class(TForm)
    backspace1: TButton;
    enter1: TButton;
    keyboard_question1: TLabel;
    num0: TButton;
    prefix1: TComboBox;
    letter1: TComboBox;
    Edit1: TEdit;
    num1: TButton;
    clear1: TButton;
    num2: TButton;
    num3: TButton;
    num5: TButton;
    num6: TButton;
    num7: TButton;
    num4: TButton;
    num8: TButton;
    num9: TButton;
    procedure backspace1Click(Sender: TObject);
    procedure clear1Click(Sender: TObject);
    procedure enter1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure letter1Change(Sender: TObject);
    procedure num7Click(Sender: TObject);
    procedure prefix1Change(Sender: TObject);
  private

  public

  end;

var
  keyboard1: Tkeyboard1;

  keyboard_caption, keyboard_question, keyboard_text  : string;

implementation

{$R *.lfm}

{ Tkeyboard1 }

procedure Tkeyboard1.num7Click(Sender: TObject);
begin
  edit1.text:=edit1.text+Tbutton(Sender).caption;
end;

procedure Tkeyboard1.prefix1Change(Sender: TObject);
begin
  edit1.Text:=edit1.Text+prefix1.Text;
end;

procedure Tkeyboard1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  keyboard_text:=edit1.text;
end;

procedure Tkeyboard1.FormShow(Sender: TObject);
begin
  caption:=keyboard_caption;
  keyboard_question1.caption:=keyboard_question;
end;

procedure Tkeyboard1.letter1Change(Sender: TObject);
begin
  edit1.Text:=edit1.Text+letter1.Text;
end;

procedure Tkeyboard1.clear1Click(Sender: TObject);
begin
  edit1.text:='';
end;

procedure Tkeyboard1.backspace1Click(Sender: TObject);
begin
  edit1.text:=copy(edit1.text,1,length(edit1.text)-1);
end;

procedure Tkeyboard1.enter1Click(Sender: TObject);
begin
  keyboard1.close;
end;

end.

