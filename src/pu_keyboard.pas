unit pu_keyboard;
{ On-screen keyboard with find object data function}
{ interface via global variables.}
{ Key functionality:
      ESC   --> close form with object_found=false

      Enter --> find object, if not found show list of designations containing the input string
                 e.g. ngc2024   leave form with ngc2024 data. Object_found=true
                      ngc       list all NGC objects
                      ngc 2024  list ngc2024 in the list
                      ngc10     leave form with ngc10 data. Object_found=true
                      ngc 10    list all objects containing ngc10 as ngc100, ngc1000, ngc1001...
                      ngc10*    list all objects containing ngc10 as ngc100, ngc1000, ngc1001...

      close form -->  2024      leave form with ngc2024 data if ngc was selected. Object_found=true
                      ngc2024   leave form with ngc2024 data. Object_found=true
                      abcdef    leave form with abcdef designation. Object_found=false
}

{
Copyright (C) 2020 Patrick Chevalley & Han Kleijn

http://www.ap-i.net
pch@ap-i.net

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  u_annotation {for deepsky database search};

type
  {Tkeyboard1 }
  Tkeyboard1 = class(TForm)
    backspace1: TButton;
    enter1: TButton;
    keyboard_question1: TLabel;
    ListBox1: TListBox;
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
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure letter1Change(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure num7Click(Sender: TObject);
    procedure prefix1Change(Sender: TObject);
  private

  public

  end;

var
  keyboard1: Tkeyboard1;

  {global variables as interface}
  keyboard_text, keyboard_caption, keyboard_question    : string;
  object_found : boolean;
  ra_data,dec_data, length_data, width_data, pa_data    :   double;

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
var
  dummy : double;
  error2: integer;
begin
  if object_found=false then {form was closed by user. Initiate find action}
  begin
    keyboard_text:=edit1.text;
    val(keyboard_text,dummy,error2);{test if this is a number or a full object name}
    if error2=0 then keyboard_text:=prefix1.text+keyboard_text; {add missing prefix}
    object_found:=find_object(keyboard_text ,ra_data,dec_data,length_data,width_data,pa_data);{keyboard_text with length less then 2 will be ignored}
  end;
end;

procedure Tkeyboard1.FormKeyPress(Sender: TObject; var Key: char);
begin
  if key=#27 {esc  } then keyboard1.close; {leave form}
  if key=#13 {enter} then enter1click(Sender); {enter}
end;

procedure Tkeyboard1.FormShow(Sender: TObject);
begin
  object_found:=false;
  caption:=keyboard_caption;
  edit1.text:=keyboard_text;
  keyboard_question1.caption:=keyboard_question;
end;

procedure Tkeyboard1.letter1Change(Sender: TObject);
begin
  edit1.Text:=edit1.Text+letter1.Text;
end;

procedure Tkeyboard1.ListBox1Click(Sender: TObject);
begin
  if  (listbox1.itemindex)>=0 then {prevent error if nothing is selected}
  edit1.text:=listbox1.Items[listbox1.itemindex];{copy selection to edit }
end;

procedure Tkeyboard1.ListBox1DblClick(Sender: TObject);
begin
  if  (listbox1.itemindex)>=0 then {prevent error if nothing is selected}
  begin
    keyboard_text:=listbox1.Items[listbox1.itemindex];{copy selection to edit }
    object_found:=true;{must be correct since it was extracted from the database}
    keyboard1.close; {double click, accept entry and close}
  end;
end;

procedure Tkeyboard1.clear1Click(Sender: TObject);
begin
  edit1.text:='';
end;

procedure Tkeyboard1.backspace1Click(Sender: TObject);
begin
  edit1.text:=copy(edit1.text,1,length(edit1.text)-1);
end;

procedure fill_listbox(filterstr: string); {fill listbox with destinations containing the filterstr}
var
  ra0,dec0,length0,width0,pa    : double;  {dummies, not used}
begin
  load_deep;{Load the deepsky database once. If already loaded, no action}
  linepos:=2;{Set pointer to the beginning}
   with keyboard1 do
  begin
    if length(filterstr)>1 then
    begin
      listbox1.Clear; { empty the list of any old values }
      repeat
        read_deepsky('T' {full database search} ,0 {ra},0 {dec},1 {cos(telescope_dec)},2*pi{fov},{var} ra0,dec0,length0,width0,pa);{Deepsky database search}
        if ((length(filterstr)=0) or (pos(filterstr,uppercase(naam2))>0)) then  listbox1.Items.Add(naam2);
        if ((length(naam3)>0)  and (((length(filterstr)=0) or (pos(filterstr,uppercase(naam3))>0)))) then listbox1.Items.Add(naam3);
        if ((length(naam4)>0)  and (((length(filterstr)=0) or (pos(filterstr,uppercase(naam4))>0)))) then listbox1.Items.Add(naam4);
      until (linepos>=$FFFFFF);{Found object or end of database} ;
      edit1.text:='';{clear filtering}

      ActiveControl:=listbox1;{set focus on listbox1 text window}
    end;
  end;
end;

procedure Tkeyboard1.enter1Click(Sender: TObject);
var
  oldCursor : TCursor;
begin
  oldCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  keyboard_text:=edit1.text;
  if find_object(keyboard_text ,ra_data,dec_data,length_data,width_data,pa_data) {find object in unit u_annotation} then
  begin
    object_found:=true;
    keyboard1.close;
  end
  else
  begin {keyboard_text with length less then 2 or not found}
    keyboard_text:=StringReplace(uppercase(keyboard_text), ' ', '',[rfReplaceAll]);{replace all space and make upcase}
    keyboard_text:=StringReplace(keyboard_text, '*', '',[rfReplaceAll]);{remove wildchard}
    fill_listbox(keyboard_text);{fill listbox with suggestions}
  end;
  Screen.Cursor := oldCursor;
end;

end.

