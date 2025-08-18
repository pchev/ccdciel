// SPDX-License-Identifier: LGPL-3.0-linking-exception

{ Layered image, each layer being a TBGRABitmap or rendered from an original.
  It can handle SVG format, gradients and blend modes. }
unit BGRALayers;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  BGRAGraphics, BGRAClasses, SysUtils, BGRABitmapTypes, BGRABitmap,
  BGRAMemDirectory, BGRATransform, fgl, BGRALayerOriginal;

type
  TBGRACustomLayeredBitmap = class;
  TBGRACustomLayeredBitmapClass = class of TBGRACustomLayeredBitmap;

  { Entry for one original in a list of originals }
  TBGRALayerOriginalEntry = record
     Guid: TGuid;
     Instance: TBGRALayerCustomOriginal;
     class operator = (const AEntry1,AEntry2: TBGRALayerOriginalEntry): boolean;
  end;

function BGRALayerOriginalEntry(AGuid: TGuid): TBGRALayerOriginalEntry;
function BGRALayerOriginalEntry(AInstance: TBGRALayerCustomOriginal): TBGRALayerOriginalEntry;

type
  TBGRALayerOriginalList = specialize TFPGList<TBGRALayerOriginalEntry>;

  TBGRALayeredBitmap = class;
  TBGRALayeredBitmapClass = class of TBGRALayeredBitmap;

  TBGRALayeredBitmapSaveToStreamProc = procedure(AStream: TStream; ALayers: TBGRACustomLayeredBitmap);
  TBGRALayeredBitmapLoadFromStreamProc = procedure(AStream: TStream; ALayers: TBGRACustomLayeredBitmap);
  TBGRALayeredBitmapCheckStreamProc = function(AStream: TStream): boolean;
  TOriginalRenderStatus = (orsNone, orsDraft, orsPartialDraft, orsProof, orsPartialProof);

  { Abstract class for storing a layered bitmap }
  TBGRACustomLayeredBitmap = class(TGraphic)
  private
    FFrozenRange: array of record
      firstLayer,lastLayer: integer;
      image: TBGRABitmap;
      linearBlend: boolean;
    end;
    FLinearBlend: boolean;
    FMemDirectory: TMemDirectory;
    FMemDirectoryOwned: boolean;
    FSelectionDrawMode: TDrawMode;
    FSelectionLayerIndex: integer;
    FSelectionRect: TRect;
    FSelectionScanner: IBGRAScanner;
    FSelectionScannerOffset: TPoint;
    function GetDefaultBlendingOperation: TBlendOperation;
    function GetHasMemFiles: boolean;
    function GetLinearBlend: boolean;
    function GetSelectionVisible: boolean;
    procedure SetLinearBlend(AValue: boolean);

  protected
    function GetNbLayers: integer; virtual; abstract;
    function GetMemDirectory: TMemDirectory;
    function GetBlendOperation(Layer: integer): TBlendOperation; virtual; abstract;
    function GetLayerVisible(layer: integer): boolean; virtual; abstract;
    function GetLayerOpacity(layer: integer): byte; virtual; abstract;
    function GetLayerName(layer: integer): string; virtual;
    function GetLayerOffset(layer: integer): TPoint; virtual;
    function GetLayerFrozenRange(layer: integer): integer;
    function GetLayerFrozen(layer: integer): boolean; virtual;
    function GetLayerUniqueId(layer: integer): integer; virtual;
    function GetLayerOriginal({%H-}layer: integer): TBGRALayerCustomOriginal; virtual;
    function GetLayerOriginalKnown({%H-}layer: integer): boolean; virtual;
    function GetLayerOriginalMatrix({%H-}layer: integer): TAffineMatrix; virtual;
    function GetLayerOriginalGuid({%H-}layer: integer): TGuid; virtual;
    function GetLayerOriginalRenderStatus({%H-}layer: integer): TOriginalRenderStatus; virtual;
    function GetOriginalCount: integer; virtual;
    function GetOriginalByIndex({%H-}AIndex: integer): TBGRALayerCustomOriginal; virtual;
    function GetOriginalByIndexKnown({%H-}AIndex: integer): boolean; virtual;
    function GetOriginalByIndexLoaded({%H-}AIndex: integer): boolean; virtual;
    function GetOriginalByIndexClass({%H-}AIndex: integer): TBGRALayerOriginalAny; virtual;
    function GetTransparent: Boolean; override;
    function GetEmpty: boolean; override;

    function IndexOfOriginal(const AGuid: TGuid): integer; overload; virtual;
    function IndexOfOriginal(AOriginal: TBGRALayerCustomOriginal): integer; overload; virtual;

    procedure SetWidth(Value: Integer); override;
    procedure SetHeight(Value: Integer); override;
    procedure SetMemDirectory(AValue: TMemDirectory);
    procedure SetTransparent(Value: Boolean); override;

    procedure SetLayerFrozen(layer: integer; AValue: boolean); virtual;
    function RangeIntersect(first1,last1,first2,last2: integer): boolean;
    procedure RemoveFrozenRange(index: integer);
    function ContainsFrozenRange(first,last: integer): boolean;
    function GetLayerDrawMode(AIndex: integer): TDrawMode;

  public
    procedure SaveToFile(const filenameUTF8: string); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure SaveToStreamAs(Stream: TStream; AExtension: string);
    constructor Create; override;
    destructor Destroy; override;
    function ToString: ansistring; override;
    procedure DiscardSelection;
    function GetLayerBitmapDirectly(layer: integer): TBGRABitmap; virtual;
    function GetLayerBitmapCopy(layer: integer): TBGRABitmap; virtual; abstract;
    function ComputeFlatImage(ASeparateXorMask: boolean = false): TBGRABitmap; overload;
    function ComputeFlatImage(firstLayer, lastLayer: integer; ASeparateXorMask: boolean = false): TBGRABitmap; overload;
    function ComputeFlatImage(ARect: TRect; ASeparateXorMask: boolean = false): TBGRABitmap; overload;
    function ComputeFlatImage(ARect: TRect; firstLayer, lastLayer: integer; ASeparateXorMask: boolean = false): TBGRABitmap; overload;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override; overload;
    procedure Draw(Canvas: TCanvas; x,y: integer); overload;
    procedure Draw(Canvas: TCanvas; x,y: integer; firstLayer, lastLayer: integer); overload;
    procedure Draw(Dest: TBGRABitmap; x,y: integer); overload;
    procedure Draw(Dest: TBGRABitmap; x,y: integer; ASeparateXorMask: boolean; ADestinationEmpty: boolean = false); overload;
    procedure Draw(Dest: TBGRABitmap; AX,AY: integer; firstLayer, lastLayer: integer; ASeparateXorMask: boolean = false; ADestinationEmpty: boolean = false); overload;
    function DrawLayer(Dest: TBGRABitmap; X,Y: Integer; AIndex: integer; ASeparateXorMask: boolean = false; ADestinationEmpty: boolean = false): boolean;

    procedure FreezeExceptOneLayer(layer: integer); overload;
    procedure Freeze(firstLayer, lastLayer: integer); overload;
    procedure Freeze; overload;
    procedure Unfreeze; overload;
    procedure Unfreeze(layer: integer); overload;
    procedure Unfreeze(firstLayer, lastLayer: integer); overload;

    procedure NotifyLoaded; virtual;
    procedure NotifySaving; virtual;

    property NbLayers: integer read GetNbLayers;
    property BlendOperation[layer: integer]: TBlendOperation read GetBlendOperation;
    property LayerVisible[layer: integer]: boolean read GetLayerVisible;
    property LayerOpacity[layer: integer]: byte read GetLayerOpacity;
    property LayerName[layer: integer]: string read GetLayerName;
    property LayerOffset[layer: integer]: TPoint read GetLayerOffset;
    property LayerFrozen[layer: integer]: boolean read GetLayerFrozen;
    property LayerUniqueId[layer: integer]: integer read GetLayerUniqueId;
    property LayerOriginal[layer: integer]: TBGRALayerCustomOriginal read GetLayerOriginal;
    property LayerOriginalKnown[layer: integer]: boolean read GetLayerOriginalKnown;
    property LayerOriginalGuid[layer: integer]: TGuid read GetLayerOriginalGuid;
    property LayerOriginalMatrix[layer: integer]: TAffineMatrix read GetLayerOriginalMatrix;
    property LayerOriginalRenderStatus[layer: integer]: TOriginalRenderStatus read GetLayerOriginalRenderStatus;
    property SelectionScanner: IBGRAScanner read FSelectionScanner write FSelectionScanner;
    property SelectionScannerOffset: TPoint read FSelectionScannerOffset write FSelectionScannerOffset;
    property SelectionRect: TRect read FSelectionRect write FSelectionRect;
    property SelectionLayerIndex: integer read FSelectionLayerIndex write FSelectionLayerIndex;
    property SelectionDrawMode: TDrawMode read FSelectionDrawMode write FSelectionDrawMode;
    property SelectionVisible: boolean read GetSelectionVisible;
    property LinearBlend: boolean read GetLinearBlend write SetLinearBlend; //use linear blending unless specified
    property DefaultBlendingOperation: TBlendOperation read GetDefaultBlendingOperation;
    property MemDirectory: TMemDirectory read GetMemDirectory write SetMemDirectory;
    property MemDirectoryOwned: boolean read FMemDirectoryOwned write FMemDirectoryOwned;
    property HasMemFiles: boolean read GetHasMemFiles;
  end;

  TEmbeddedOriginalChangeEvent = procedure (ASender: TObject; AOriginal: TBGRALayerCustomOriginal;
                                            var ADiff: TBGRAOriginalDiff) of object;
  TEmbeddedOriginalEditingChangeEvent = procedure (ASender: TObject; AOriginal: TBGRALayerCustomOriginal) of object;
  TLayeredActionProgressEvent = procedure(ASender: TObject; AProgressPercent: integer) of object;
  TEmbeddedOriginalLoadErrorEvent = procedure (ASender: TObject; AError: string; var ARaise: boolean) of object;

  { Information about one layer }
  TBGRALayerInfo = record
    UniqueId: integer;
    Name: string;
    x, y: integer;
    Source: TBGRABitmap;
    blendOp: TBlendOperation;
    Opacity: byte;
    Visible: boolean;
    Owner: boolean;
    Frozen: boolean;
    OriginalMatrix: TAffineMatrix;
    OriginalRenderStatus: TOriginalRenderStatus;
    OriginalGuid: TGuid;
    OriginalInvalidatedBounds: TRectF;
  end;

  { Base implementation for a layered bitmap }
  TBGRALayeredBitmap = class(TBGRACustomLayeredBitmap)
  private
    FNbLayers: integer;
    FLayers: array of TBGRALayerInfo;
    FOnActionDone: TNotifyEvent;
    FOnEditorFocusChanged: TNotifyEvent;
    FEditorFocused: boolean;
    FOnActionProgress: TLayeredActionProgressEvent;
    FOnOriginalLoadError: TEmbeddedOriginalLoadErrorEvent;
    FOriginalChange: TEmbeddedOriginalChangeEvent;
    FOriginalEditingChange: TEmbeddedOriginalEditingChangeEvent;
    FWidth,FHeight: integer;
    FOriginals: TBGRALayerOriginalList;
    FOriginalEditor: TBGRAOriginalEditor;
    FOriginalEditorOriginal: TGuid;
    FOriginalEditorViewMatrix: TAffineMatrix;
    procedure EditorFocusedChanged({%H-}Sender: TObject);
    function GetLayerOriginalClass(layer: integer): TBGRALayerOriginalAny;
    function GetOriginalEditor: TBGRAOriginalEditor;
    function GetOriginalGuid(AIndex: integer): TGUID;
    procedure SetEditorFocused(AValue: boolean);
    procedure SetOnActionDone(AValue: TNotifyEvent);
    procedure SetOnActionProgress(AValue: TLayeredActionProgressEvent);

  protected
    function GetWidth: integer; override;
    function GetHeight: integer; override;
    function GetNbLayers: integer; override;
    function GetBlendOperation(Layer: integer): TBlendOperation; override;
    function GetLayerVisible(layer: integer): boolean; override;
    function GetLayerOpacity(layer: integer): byte; override;
    function GetLayerOffset(layer: integer): TPoint; override;
    function GetLayerName(layer: integer): string; override;
    function GetLayerFrozen(layer: integer): boolean; override;
    function GetLayerUniqueId(layer: integer): integer; override;
    function GetLayerOriginal(layer: integer): TBGRALayerCustomOriginal; override;
    function GetLayerOriginalKnown(layer: integer): boolean; override;
    function GetLayerOriginalMatrix(layer: integer): TAffineMatrix; override;
    function GetLayerOriginalGuid(layer: integer): TGuid; override;
    function GetLayerOriginalRenderStatus(layer: integer): TOriginalRenderStatus; override;
    function GetOriginalCount: integer; override;
    function GetOriginalByIndex(AIndex: integer): TBGRALayerCustomOriginal; override;
    function GetOriginalByIndexKnown(AIndex: integer): boolean; override;
    function GetOriginalByIndexLoaded(AIndex: integer): boolean; override;
    function GetOriginalByIndexClass(AIndex: integer): TBGRALayerOriginalAny; override;
    procedure SetBlendOperation(Layer: integer; op: TBlendOperation);
    procedure SetLayerVisible(layer: integer; AValue: boolean);
    procedure SetLayerOpacity(layer: integer; AValue: byte);
    procedure SetLayerOffset(layer: integer; AValue: TPoint);
    procedure SetLayerName(layer: integer; AValue: string);
    procedure SetLayerFrozen(layer: integer; AValue: boolean); override;
    procedure SetLayerUniqueId(layer: integer; AValue: integer);
    procedure SetLayerOriginalMatrix(layer: integer; AValue: TAffineMatrix);
    procedure SetLayerOriginalGuid(layer: integer; const AValue: TGuid);
    procedure SetLayerOriginalRenderStatus(layer: integer; AValue: TOriginalRenderStatus);

    procedure FindOriginal(AGuid: TGuid;
                out ADir: TMemDirectory;
                out AClass: TBGRALayerOriginalAny);
    procedure StoreOriginal(AOriginal: TBGRALayerCustomOriginal);
    procedure OriginalChange(ASender: TObject; ABounds: PRectF; var ADiff: TBGRAOriginalDiff);
    procedure OriginalEditingChange(ASender: TObject);
    function GetLayerDirectory(ALayerIndex: integer; ACanCreate: boolean): TMemDirectory;
    procedure UpdateOriginalEditor(ALayerIndex: integer; AMatrix: TAffineMatrix;
      APointSize: single);

  public
    procedure LoadFromFile(const filenameUTF8: string); override;
    procedure LoadFromStream(stream: TStream); override;
    procedure LoadFromResource(AFilename: string);
    procedure SetSize(AWidth, AHeight: integer); virtual;
    procedure Clear; override;
    procedure ClearOriginals;
    procedure RemoveLayer(index: integer);
    procedure InsertLayer(index: integer; fromIndex: integer);
    procedure Assign(ASource: TBGRACustomLayeredBitmap; ASharedLayerIds: boolean = false;
                ACopyAdditionalMemData: boolean = false); overload;
    function MoveLayerUp(index: integer): integer;
    function MoveLayerDown(index: integer): integer;

    function AddLayer(Source: TBGRABitmap; Opacity: byte = 255): integer; overload;
    function AddLayer(Source: TBGRABitmap; Position: TPoint; BlendOp: TBlendOperation; Opacity: byte = 255; Shared: boolean = false): integer; overload;
    function AddLayer(Source: TBGRABitmap; Position: TPoint; Opacity: byte = 255): integer; overload;
    function AddLayer(Source: TBGRABitmap; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;
    function AddLayer(AName: string; Source: TBGRABitmap; Opacity: byte = 255): integer; overload;
    function AddLayer(AName: string; Source: TBGRABitmap; Position: TPoint; BlendOp: TBlendOperation; Opacity: byte = 255; Shared: boolean = false): integer; overload;
    function AddLayer(AName: string; Source: TBGRABitmap; Position: TPoint; Opacity: byte = 255): integer; overload;
    function AddLayer(AName: string; Source: TBGRABitmap; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;
    function AddSharedLayer(Source: TBGRABitmap; Opacity: byte = 255): integer; overload;
    function AddSharedLayer(Source: TBGRABitmap; Position: TPoint; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;
    function AddSharedLayer(Source: TBGRABitmap; Position: TPoint; Opacity: byte = 255): integer; overload;
    function AddSharedLayer(Source: TBGRABitmap; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;
    function AddLayerFromFile(AFileName: string; Opacity: byte = 255): integer; overload;
    function AddLayerFromFile(AFileName: string; Position: TPoint; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;
    function AddLayerFromFile(AFileName: string; Position: TPoint; Opacity: byte = 255): integer; overload;
    function AddLayerFromFile(AFileName: string; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;
    function AddOwnedLayer(ABitmap: TBGRABitmap; Opacity: byte = 255): integer; overload;
    function AddOwnedLayer(ABitmap: TBGRABitmap; Position: TPoint; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;
    function AddOwnedLayer(ABitmap: TBGRABitmap; Position: TPoint; Opacity: byte = 255): integer; overload;
    function AddOwnedLayer(ABitmap: TBGRABitmap; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;
    function AddLayerFromOriginal(const AGuid: TGuid; Opacity: byte = 255): integer; overload;
    function AddLayerFromOriginal(const AGuid: TGuid; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;
    function AddLayerFromOriginal(const AGuid: TGuid; Matrix: TAffineMatrix; Opacity: byte = 255): integer; overload;
    function AddLayerFromOriginal(const AGuid: TGuid; Matrix: TAffineMatrix; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;
    function AddLayerFromOwnedOriginal(AOriginal: TBGRALayerCustomOriginal; Opacity: byte = 255): integer; overload;
    function AddLayerFromOwnedOriginal(AOriginal: TBGRALayerCustomOriginal; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;
    function AddLayerFromOwnedOriginal(AOriginal: TBGRALayerCustomOriginal; Matrix: TAffineMatrix; Opacity: byte = 255): integer; overload;
    function AddLayerFromOwnedOriginal(AOriginal: TBGRALayerCustomOriginal; Matrix: TAffineMatrix; BlendOp: TBlendOperation; Opacity: byte = 255): integer; overload;

    class function IsValidRegistryIndentifier(AIdentifier: string): boolean;
    function GetLayerRegistry(ALayerIndex: integer; ARegistryIdentifier: string): RawByteString;
    procedure SetLayerRegistry(ALayerIndex: integer; ARegistryIdentifier: string; AValue: RawByteString);
    procedure SaveLayerRegistryToStream(ALayerIndex: integer; AStream: TStream);
    procedure LoadLayerRegistryFromStream(ALayerIndex: integer; AStream: TStream);
    function GetGlobalRegistry(ARegistryIdentifier: string): RawByteString;
    procedure SetGlobalRegistry(ARegistryIdentifier: string; AValue: RawByteString);

    function AddOriginal(AOriginal: TBGRALayerCustomOriginal; AOwned: boolean = true): integer;
    function AddOriginalFromStream(AStream: TStream; ALateLoad: boolean = false): integer; overload;
    function AddOriginalFromStream(AStream: TStream; const AGuid: TGuid; ALateLoad: boolean = false): integer; overload;
    function AddOriginalFromStorage(AStorage: TBGRAMemOriginalStorage; ALateLoad: boolean = false): integer; overload;
    function AddOriginalFromStorage(AStorage: TBGRAMemOriginalStorage; const AGuid: TGuid; ALateLoad: boolean = false): integer; overload;
    procedure SaveOriginalToStream(AIndex: integer; AStream: TStream); overload;
    procedure SaveOriginalToStream(const AGuid: TGuid; AStream: TStream); overload;
    function RemoveOriginal(AOriginal: TBGRALayerCustomOriginal): boolean;
    procedure DeleteOriginal(AIndex: integer);
    procedure NotifyLoaded; override;
    procedure NotifySaving; override;
    procedure RenderLayerFromOriginal(layer: integer; ADraft: boolean = false; AFullSizeLayer: boolean = false); overload;
    procedure RenderLayerFromOriginal(layer: integer; ADraft: boolean; ARenderBounds: TRect; AFullSizeLayer: boolean = false); overload;
    procedure RenderLayerFromOriginal(layer: integer; ADraft: boolean; ARenderBoundsF: TRectF; AFullSizeLayer: boolean = false); overload;
    procedure RenderLayerFromOriginalIfNecessary(layer: integer; ADraft: boolean; var ABounds: TRect);
    function RenderOriginalsIfNecessary(ADraft: boolean = false): TRect;
    function RenderOriginalIfNecessary(const AGuid: TGuid; ADraft: boolean = false): TRect;
    procedure RemoveUnusedOriginals;
    procedure UnloadOriginals;
    procedure UnloadOriginal(AIndex: integer); overload;
    procedure UnloadOriginal(const AGuid: TGuid); overload;

    destructor Destroy; override;
    constructor Create; overload; override;
    constructor Create(AWidth, AHeight: integer); overload; virtual;
    function GetLayerBitmapDirectly(layer: integer): TBGRABitmap; override;
    function GetLayerBitmapCopy(layer: integer): TBGRABitmap; override;
    function GetLayerIndexFromId(AIdentifier: integer): integer;
    function Duplicate(ASharedLayerIds: boolean = false): TBGRALayeredBitmap;
    function ProduceLayerUniqueId: integer;

    procedure RotateCW;
    procedure RotateCCW;
    procedure RotateUD; overload;
    procedure RotateUD(ALayerIndex: integer); overload;
    procedure HorizontalFlip; overload;
    procedure HorizontalFlip(ALayerIndex: integer); overload;
    procedure VerticalFlip; overload;
    procedure VerticalFlip(ALayerIndex: integer); overload;
    procedure Resample(AWidth, AHeight: integer; AResampleMode: TResampleMode; AFineResampleFilter: TResampleFilter = rfLinear);
    procedure SetLayerBitmap(layer: integer; ABitmap: TBGRABitmap; AOwned: boolean);
    function TakeLayerBitmap(layer: integer): TBGRABitmap;
    procedure ApplyLayerOffset(ALayerIndex: integer; APadWithTranparentPixels: boolean);

    function DrawEditor(ADest: TBGRABitmap; ALayerIndex: integer; X, Y: Integer; APointSize: single): TRect; overload;
    function DrawEditor(ADest: TBGRABitmap; ALayerIndex: integer; AMatrix: TAffineMatrix; APointSize: single): TRect; overload;
    function GetEditorBounds(ALayerIndex: integer; X, Y: Integer; APointSize: single): TRect; overload;
    function GetEditorBounds(ADestRect: TRect; ALayerIndex: integer; X, Y: Integer; APointSize: single): TRect; overload;
    function GetEditorBounds(ALayerIndex: integer; AMatrix: TAffineMatrix; APointSize: single): TRect; overload;
    function GetEditorBounds(ADestRect: TRect; ALayerIndex: integer; AMatrix: TAffineMatrix; APointSize: single): TRect; overload;
    procedure ClearEditor;
    procedure MouseMove(Shift: TShiftState; ImageX, ImageY: Single; out ACursor: TOriginalEditorCursor);
    procedure MouseDown(RightButton: boolean; Shift: TShiftState; ImageX, ImageY: Single; out ACursor: TOriginalEditorCursor);
    procedure MouseUp(RightButton: boolean; Shift: TShiftState; ImageX, ImageY: Single; out ACursor: TOriginalEditorCursor);
    procedure MouseMove(Shift: TShiftState; ImageX, ImageY: Single; out ACursor: TOriginalEditorCursor; out AHandled: boolean);
    procedure MouseDown(RightButton: boolean; Shift: TShiftState; ImageX, ImageY: Single; out ACursor: TOriginalEditorCursor; out AHandled: boolean);
    procedure MouseUp(RightButton: boolean; Shift: TShiftState; ImageX, ImageY: Single; out ACursor: TOriginalEditorCursor; out AHandled: boolean);
    procedure KeyDown(Shift: TShiftState; Key: TSpecialKey; out AHandled: boolean);
    procedure KeyUp(Shift: TShiftState; Key: TSpecialKey; out AHandled: boolean);
    procedure KeyPress(UTF8Key: string; out AHandled: boolean);

    property Width : integer read GetWidth;
    property Height: integer read GetHeight;
    property NbLayers: integer read GetNbLayers;
    property BlendOperation[layer: integer]: TBlendOperation read GetBlendOperation write SetBlendOperation;
    property LayerVisible[layer: integer]: boolean read GetLayerVisible write SetLayerVisible;
    property LayerOpacity[layer: integer]: byte read GetLayerOpacity write SetLayerOpacity;
    property LayerName[layer: integer]: string read GetLayerName write SetLayerName;
    property LayerBitmap[layer: integer]: TBGRABitmap read GetLayerBitmapDirectly;
    property LayerOffset[layer: integer]: TPoint read GetLayerOffset write SetLayerOffset;
    property LayerUniqueId[layer: integer]: integer read GetLayerUniqueId write SetLayerUniqueId;
    property LayerOriginal[layer: integer]: TBGRALayerCustomOriginal read GetLayerOriginal;
    property LayerOriginalKnown[layer: integer]: boolean read GetLayerOriginalKnown;
    property LayerOriginalClass[layer: integer]: TBGRALayerOriginalAny read GetLayerOriginalClass;
    property LayerOriginalGuid[layer: integer]: TGuid read GetLayerOriginalGuid write SetLayerOriginalGuid;
    property LayerOriginalMatrix[layer: integer]: TAffineMatrix read GetLayerOriginalMatrix write SetLayerOriginalMatrix;
    property LayerOriginalRenderStatus[layer: integer]: TOriginalRenderStatus read GetLayerOriginalRenderStatus write SetLayerOriginalRenderStatus;

    function IndexOfOriginal(const AGuid: TGuid): integer; overload; override;
    function IndexOfOriginal(AOriginal: TBGRALayerCustomOriginal): integer; overload; override;
    property OriginalCount: integer read GetOriginalCount;
    property Original[AIndex: integer]: TBGRALayerCustomOriginal read GetOriginalByIndex;
    property OriginalGuid[AIndex: integer]: TGUID read GetOriginalGuid;
    property OriginalKnown[AIndex: integer]: boolean read GetOriginalByIndexKnown;
    property OriginalClass[AIndex: integer]: TBGRALayerOriginalAny read GetOriginalByIndexClass;
    property OnOriginalChange: TEmbeddedOriginalChangeEvent read FOriginalChange write FOriginalChange;
    property OnOriginalEditingChange: TEmbeddedOriginalEditingChangeEvent read FOriginalEditingChange write FOriginalEditingChange;
    property OnOriginalLoadError: TEmbeddedOriginalLoadErrorEvent read FOnOriginalLoadError write FOnOriginalLoadError;
    property EditorFocused: boolean read FEditorFocused write SetEditorFocused;
    property OnEditorFocusChanged: TNotifyEvent read FOnEditorFocusChanged write FOnEditorFocusChanged;
    property OriginalEditor: TBGRAOriginalEditor read GetOriginalEditor;
    property OnActionProgress: TLayeredActionProgressEvent read FOnActionProgress write SetOnActionProgress;
    property OnActionDone: TNotifyEvent read FOnActionDone write SetOnActionDone;
  end;

  TAffineMatrix = BGRABitmapTypes.TAffineMatrix;

procedure RegisterLayeredBitmapWriter(AExtensionUTF8: string; AWriter: TBGRALayeredBitmapClass);
procedure RegisterLayeredBitmapReader(AExtensionUTF8: string; AReader: TBGRACustomLayeredBitmapClass);
function TryCreateLayeredBitmapWriter(AExtensionUTF8: string): TBGRALayeredBitmap;
function TryCreateLayeredBitmapReader(AExtensionUTF8: string): TBGRACustomLayeredBitmap;

var
  LayeredBitmapSaveToStreamProc : TBGRALayeredBitmapSaveToStreamProc;
  LayeredBitmapLoadFromStreamProc : TBGRALayeredBitmapLoadFromStreamProc;
  LayeredBitmapCheckStreamProc: TBGRALayeredBitmapCheckStreamProc;

type
  TOnLayeredBitmapLoadStartProc = procedure(AFilenameUTF8: string) of object;
  TOnLayeredBitmapLoadProgressProc = procedure(APercentage: integer) of object;
  TOnLayeredBitmapLoadedProc = procedure() of object;

procedure OnLayeredBitmapLoadFromStreamStart;
procedure OnLayeredBitmapLoadStart(AFilenameUTF8: string);
procedure OnLayeredBitmapLoadProgress(APercentage: integer);
procedure OnLayeredBitmapLoaded;
procedure RegisterLoadingHandler(AStart: TOnLayeredBitmapLoadStartProc; AProgress: TOnLayeredBitmapLoadProgressProc;
     ADone: TOnLayeredBitmapLoadedProc);
procedure UnregisterLoadingHandler(AStart: TOnLayeredBitmapLoadStartProc; AProgress: TOnLayeredBitmapLoadProgressProc;
     ADone: TOnLayeredBitmapLoadedProc);

type
  TOnLayeredBitmapSaveStartProc = procedure(AFilenameUTF8: string) of object;
  TOnLayeredBitmapSaveProgressProc = procedure(APercentage: integer) of object;
  TOnLayeredBitmapSavedProc = procedure() of object;

procedure OnLayeredBitmapSaveToStreamStart;
procedure OnLayeredBitmapSaveStart(AFilenameUTF8: string);
procedure OnLayeredBitmapSaveProgress(APercentage: integer);
procedure OnLayeredBitmapSaved;
procedure RegisterSavingHandler(AStart: TOnLayeredBitmapSaveStartProc; AProgress: TOnLayeredBitmapSaveProgressProc;
     ADone: TOnLayeredBitmapSavedProc);
procedure UnregisterSavingHandler(AStart: TOnLayeredBitmapSaveStartProc; AProgress: TOnLayeredBitmapSaveProgressProc;
     ADone: TOnLayeredBitmapSavedProc);

const
  RenderTempSubDirectory = 'temp';

implementation

uses BGRAUTF8, BGRABlend, BGRAMultiFileType, math;

const
  OriginalsDirectory = 'originals';
  LayersDirectory = 'layers';
  RenderSubDirectory = 'render';
  RegistrySubDirectory = 'registry';

type
  TOnLayeredBitmapLoadStartProcList = specialize TFPGList<TOnLayeredBitmapLoadStartProc>;
  TOnLayeredBitmapLoadProgressProcList = specialize TFPGList<TOnLayeredBitmapLoadProgressProc>;
  TOnLayeredBitmapLoadedProcList = specialize TFPGList<TOnLayeredBitmapLoadedProc>;
  TOnLayeredBitmapSaveStartProcList = specialize TFPGList<TOnLayeredBitmapSaveStartProc>;
  TOnLayeredBitmapSaveProgressProcList = specialize TFPGList<TOnLayeredBitmapSaveProgressProc>;
  TOnLayeredBitmapSavedProcList = specialize TFPGList<TOnLayeredBitmapSavedProc>;

var
  LayeredBitmapLoadEvents: record
    OnStart: TOnLayeredBitmapLoadStartProcList;
    OnProgress: TOnLayeredBitmapLoadProgressProcList;
    OnDone: TOnLayeredBitmapLoadedProcList;
  end;
  LayeredBitmapSaveEvents: record
    OnStart: TOnLayeredBitmapSaveStartProcList;
    OnProgress: TOnLayeredBitmapSaveProgressProcList;
    OnDone: TOnLayeredBitmapSavedProcList;
  end;

var
  NextLayerUniqueId: LongWord;
  LayeredBitmapReaders: array of record
     extension: string;
     theClass: TBGRACustomLayeredBitmapClass;
  end;
  LayeredBitmapWriters: array of record
     extension: string;
     theClass: TBGRALayeredBitmapClass;
  end;

{ TBGRALayerOriginalEntry }

class operator TBGRALayerOriginalEntry.=(const AEntry1,
  AEntry2: TBGRALayerOriginalEntry): boolean;
begin
  result := AEntry1.Guid = AEntry2.Guid;
end;

function BGRALayerOriginalEntry(AGuid: TGuid): TBGRALayerOriginalEntry;
begin
  result.Guid := AGuid;
  result.Instance := nil;
end;

function BGRALayerOriginalEntry(AInstance: TBGRALayerCustomOriginal): TBGRALayerOriginalEntry;
begin
  result.Guid := AInstance.Guid;
  result.Instance := AInstance;
end;

{ TBGRALayeredBitmap }

function TBGRALayeredBitmap.GetLayerUniqueId(layer: integer): integer;
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
    Result:= FLayers[layer].UniqueId;
end;

function TBGRALayeredBitmap.GetLayerOriginal(layer: integer): TBGRALayerCustomOriginal;
var
  idxOrig: Integer;
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
  begin
    if FLayers[layer].OriginalGuid = GUID_NULL then exit(nil);
    idxOrig := IndexOfOriginal(FLayers[layer].OriginalGuid);
    if idxOrig = -1 then exit(nil);
    result := Original[idxOrig];
  end;
end;

function TBGRALayeredBitmap.GetLayerOriginalMatrix(layer: integer
  ): TAffineMatrix;
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
    result := FLayers[layer].OriginalMatrix;
end;

function TBGRALayeredBitmap.GetLayerOriginalGuid(layer: integer): TGuid;
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
    result := FLayers[layer].OriginalGuid;
end;

function TBGRALayeredBitmap.GetLayerOriginalRenderStatus(layer: integer
  ): TOriginalRenderStatus;
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
    result := FLayers[layer].OriginalRenderStatus;
end;

procedure TBGRALayeredBitmap.SetLayerUniqueId(layer: integer; AValue: integer);
var i: integer;
  layerDir: TMemDirectory;
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
  begin
    for i := 0 to NbLayers-1 do
      if (i <> layer) and (FLayers[i].UniqueId = AValue) then
        raise Exception.Create('Another layer has the same identifier');
    layerDir := GetLayerDirectory(layer,false);
    if Assigned(layerDir) then
      layerDir.ParentDirectory.Rename(inttostr(FLayers[layer].UniqueId),'',inttostr(AValue));
    FLayers[layer].UniqueId := AValue;
  end;
end;

procedure TBGRALayeredBitmap.SetLayerOriginalMatrix(layer: integer;
  AValue: TAffineMatrix);
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
  begin
    if FLayers[layer].OriginalMatrix = AValue then exit;
    FLayers[layer].OriginalMatrix := AValue;
    if FLayers[layer].OriginalGuid <> GUID_NULL then
    begin
      FLayers[layer].OriginalRenderStatus := orsNone;
      Unfreeze(layer);
    end;
  end;
end;

procedure TBGRALayeredBitmap.SetLayerOriginalGuid(layer: integer;
  const AValue: TGuid);
var
  layerDir: TMemDirectory;
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
  begin
    if FLayers[layer].OriginalGuid = AValue then exit;
    FLayers[layer].OriginalGuid := AValue;
    layerDir := GetLayerDirectory(layer, false);
    if Assigned(layerDir) then
      layerDir.Delete(RenderSubDirectory,'');

    if (AValue <> GUID_NULL) and (IndexOfOriginal(AValue) <> -1) then
    begin
      FLayers[layer].OriginalRenderStatus := orsNone;
      Unfreeze(layer);
    end;
  end;
end;

procedure TBGRALayeredBitmap.SetLayerOriginalRenderStatus(layer: integer;
  AValue: TOriginalRenderStatus);
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
  begin
    if FLayers[layer].OriginalRenderStatus = AValue then exit;
    FLayers[layer].OriginalRenderStatus := AValue;
    Unfreeze(layer);
  end;
end;

procedure TBGRALayeredBitmap.FindOriginal(AGuid: TGuid; out
  ADir: TMemDirectory; out AClass: TBGRALayerOriginalAny);
var
  c: String;
begin
  ADir := nil;
  AClass := nil;

  if HasMemFiles then
  begin
    ADir := MemDirectory.FindPath(OriginalsDirectory+'/'+GUIDToString(AGuid));
    if ADir <> nil then
    begin
      c := ADir.RawStringByFilename['class'];
      AClass := FindLayerOriginalClass(c);
    end;
  end;
end;

procedure TBGRALayeredBitmap.StoreOriginal(AOriginal: TBGRALayerCustomOriginal);
var
  dir, subdir: TMemDirectory;
  storage: TBGRAMemOriginalStorage;
begin
  if AOriginal.Guid = GUID_NULL then raise exception.Create('Original GUID undefined');
  dir := MemDirectory.Directory[MemDirectory.AddDirectory(OriginalsDirectory)];
  subdir := dir.Directory[dir.AddDirectory(GUIDToString(AOriginal.Guid))];
  storage := TBGRAMemOriginalStorage.Create(subdir);
  try
    AOriginal.SaveToStorage(storage);
    storage.RawString['class'] := AOriginal.StorageClassName;
  finally
    storage.Free;
  end;
end;

procedure TBGRALayeredBitmap.OriginalChange(ASender: TObject; ABounds: PRectF; var ADiff: TBGRAOriginalDiff);
var
  i: Integer;
  orig: TBGRALayerCustomOriginal;
  transfBounds: TRectF;
begin
  orig := TBGRALayerCustomOriginal(ASender);
  if not (Assigned(ABounds) and IsEmptyRectF(ABounds^)) then
  begin
    for i := 0 to NbLayers-1 do
      if LayerOriginalGuid[i] = orig.Guid then
      begin
        if ABounds = nil then
          LayerOriginalRenderStatus[i] := orsNone
        else
        begin
          transfBounds := (LayerOriginalMatrix[i]*TAffineBox.AffineBox(ABounds^)).RectBoundsF;
          case LayerOriginalRenderStatus[i] of
          orsDraft: begin
                      LayerOriginalRenderStatus[i] := orsPartialDraft;
                      FLayers[i].OriginalInvalidatedBounds := transfBounds;
                    end;
          orsProof: begin
                      LayerOriginalRenderStatus[i] := orsPartialProof;
                      FLayers[i].OriginalInvalidatedBounds := transfBounds;
                    end;
          orsPartialDraft: FLayers[i].OriginalInvalidatedBounds :=
                             FLayers[i].OriginalInvalidatedBounds.Union(transfBounds, true);
          orsPartialProof: FLayers[i].OriginalInvalidatedBounds :=
                             FLayers[i].OriginalInvalidatedBounds.Union(transfBounds, true);
          end;
        end;
      end;
  end;
  if Assigned(FOriginalChange) then
    FOriginalChange(self, orig, ADiff);
end;

procedure TBGRALayeredBitmap.OriginalEditingChange(ASender: TObject);
var
  orig: TBGRALayerCustomOriginal;
begin
  orig := TBGRALayerCustomOriginal(ASender);
  if Assigned(FOriginalEditingChange) then
    FOriginalEditingChange(self, orig);
end;

function TBGRALayeredBitmap.GetLayerDirectory(ALayerIndex: integer; ACanCreate: boolean): TMemDirectory;
var
  layersDir: TMemDirectory;
  id: LongInt;
begin
  if (MemDirectory.IndexOf(LayersDirectory,'')=-1) and not ACanCreate then exit(nil);
  layersDir := MemDirectory.Directory[MemDirectory.AddDirectory(LayersDirectory)];
  id := LayerUniqueId[ALayerIndex];
  if (layersDir.IndexOf(IntToStr(id),'')=-1) and not ACanCreate then exit(nil);
  result := layersDir.Directory[layersDir.AddDirectory(IntToStr(id))];
end;

procedure TBGRALayeredBitmap.UpdateOriginalEditor(ALayerIndex: integer; AMatrix: TAffineMatrix; APointSize: single);
var
  orig: TBGRALayerCustomOriginal;
  editMatrix: TAffineMatrix;
begin
  orig := LayerOriginal[ALayerIndex];

  if (orig = nil) or (orig.Guid <> FOriginalEditorOriginal) then
  begin
    FreeAndNil(FOriginalEditor);
    if orig = nil then
      FOriginalEditorOriginal := GUID_NULL
      else FOriginalEditorOriginal := orig.Guid;
  end;

  if Assigned(OriginalEditor) then
    FOriginalEditor.Clear;

  if Assigned(orig) then
  begin
    if OriginalEditor = nil then
    begin
      FOriginalEditor := orig.CreateEditor;
      if FOriginalEditor = nil then
        raise exception.Create('Unexpected nil value');
      FOriginalEditor.Focused := FEditorFocused;
      FOriginalEditor.OnFocusChanged:=@EditorFocusedChanged;
    end;

    editMatrix := AffineMatrixTranslation(-0.5,-0.5)*AMatrix*LayerOriginalMatrix[ALayerIndex]*AffineMatrixTranslation(0.5,0.5);
    if IsAffineMatrixInversible(editMatrix) then
    begin
      orig.ConfigureEditor(FOriginalEditor);
      FOriginalEditorViewMatrix := AffineMatrixTranslation(-0.5,-0.5)*AMatrix*AffineMatrixTranslation(0.5,0.5);
      FOriginalEditor.Matrix := editMatrix;
      FOriginalEditor.PointSize := APointSize;
    end;
  end;
end;

function TBGRALayeredBitmap.GetOriginalCount: integer;
begin
  if Assigned(FOriginals) then
    result := FOriginals.Count
  else
    result := 0;
end;

function TBGRALayeredBitmap.GetOriginalByIndex(AIndex: integer
  ): TBGRALayerCustomOriginal;
var
  dir: TMemDirectory;
  c: TBGRALayerOriginalAny;
  guid: TGuid;
  storage: TBGRAMemOriginalStorage;
  raiseError: Boolean;
begin
  if (AIndex < 0) or (AIndex >= OriginalCount) then
    raise ERangeError.Create('Index out of bounds');

  result := FOriginals[AIndex].Instance;
  guid := FOriginals[AIndex].Guid;

  // load original on the fly
  if (result = nil) and (guid <> GUID_NULL) then
  begin
    FindOriginal(guid, dir, c);
    if not Assigned(dir) then
      raise exception.Create('Original directory not found');
    if not Assigned(c) then
      raise exception.Create('Original class not found (it can be registered with the RegisterLayerOriginal function)');

    result := c.Create;
    result.Guid := guid;
    storage := TBGRAMemOriginalStorage.Create(dir);
    try
      try
        result.LoadFromStorage(storage);
      finally
        FOriginals[AIndex] := BGRALayerOriginalEntry(result);
        result.OnChange:= @OriginalChange;
        result.OnEditingChange:= @OriginalEditingChange;
        storage.Free;
      end;
    except
      on ex: Exception do
      begin
        raiseError := true;
        if Assigned(FOnOriginalLoadError) then
          FOnOriginalLoadError(self, ex.Message, raiseError);
        if raiseError then
          raise ex;
      end;
    end;
  end;
end;

function TBGRALayeredBitmap.GetLayerOriginalKnown(layer: integer): boolean;
var
  idxOrig: Integer;
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
  begin
    if FLayers[layer].OriginalGuid = GUID_NULL then exit(true);
    idxOrig := IndexOfOriginal(FLayers[layer].OriginalGuid);
    if idxOrig = -1 then exit(false);
    result := OriginalKnown[idxOrig];
  end;
end;

function TBGRALayeredBitmap.GetOriginalByIndexKnown(AIndex: integer): boolean;
var
  dir: TMemDirectory;
  c: TBGRALayerOriginalAny;
  guid: TGuid;
begin
  if (AIndex < 0) or (AIndex >= OriginalCount) then
    raise ERangeError.Create('Index out of bounds');

  if Assigned(FOriginals[AIndex].Instance) then exit(true);
  guid := FOriginals[AIndex].Guid;
  if guid = GUID_NULL then exit(true);

  FindOriginal(guid, dir, c);
  result:= Assigned(dir) and Assigned(c);
end;

function TBGRALayeredBitmap.GetOriginalByIndexLoaded(AIndex: integer): boolean;
begin
  if (AIndex < 0) or (AIndex >= OriginalCount) then
    raise ERangeError.Create('Index out of bounds');

  Result:= Assigned(FOriginals[AIndex].Instance);
end;

function TBGRALayeredBitmap.GetOriginalGuid(AIndex: integer): TGUID;
begin
  if (AIndex < 0) or (AIndex >= OriginalCount) then
    raise ERangeError.Create('Index out of bounds');

  result := FOriginals[AIndex].Guid;
end;

procedure TBGRALayeredBitmap.SetEditorFocused(AValue: boolean);
begin
  if Assigned(OriginalEditor) then OriginalEditor.Focused := AValue
  else
  begin
    if FEditorFocused=AValue then Exit;
    FEditorFocused:=AValue;
    if Assigned(FOnEditorFocusChanged) then FOnEditorFocusChanged(self);
  end;
end;

procedure TBGRALayeredBitmap.SetOnActionDone(AValue: TNotifyEvent);
begin
  if FOnActionDone=AValue then Exit;
  FOnActionDone:=AValue;
end;

procedure TBGRALayeredBitmap.SetOnActionProgress(
  AValue: TLayeredActionProgressEvent);
begin
  if FOnActionProgress=AValue then Exit;
  FOnActionProgress:=AValue;
end;

function TBGRALayeredBitmap.GetLayerOriginalClass(layer: integer): TBGRALayerOriginalAny;
var
  idxOrig: Integer;
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
  begin
    if FLayers[layer].OriginalGuid = GUID_NULL then exit(nil);
    idxOrig := IndexOfOriginal(FLayers[layer].OriginalGuid);
    if idxOrig = -1 then exit(nil);
    result := OriginalClass[idxOrig];
  end;
end;

function TBGRALayeredBitmap.GetOriginalEditor: TBGRAOriginalEditor;
begin
  if Assigned(FOriginalEditor) and (IndexOfOriginal(FOriginalEditorOriginal)=-1) then
  begin
    FreeAndNil(FOriginalEditor);
    FOriginalEditorOriginal := GUID_NULL;
  end;
  result := FOriginalEditor;
end;

procedure TBGRALayeredBitmap.EditorFocusedChanged(Sender: TObject);
begin
  if Assigned(OriginalEditor) then
  begin
    FEditorFocused := OriginalEditor.Focused;
    if Assigned(FOnEditorFocusChanged) then FOnEditorFocusChanged(self);
  end;
end;

function TBGRALayeredBitmap.GetOriginalByIndexClass(AIndex: integer): TBGRALayerOriginalAny;
var
  dir: TMemDirectory;
  c: TBGRALayerOriginalAny;
  guid: TGuid;
begin
  if (AIndex < 0) or (AIndex >= OriginalCount) then
    raise ERangeError.Create('Index out of bounds');

  if Assigned(FOriginals[AIndex].Instance) then exit(TBGRALayerOriginalAny(FOriginals[AIndex].Instance.ClassType));
  guid := FOriginals[AIndex].Guid;
  if guid = GUID_NULL then exit(nil);

  FindOriginal(guid, dir, c);
  result:= c;
end;

function TBGRALayeredBitmap.GetWidth: integer;
begin
  Result:= FWidth;
end;

function TBGRALayeredBitmap.GetHeight: integer;
begin
  Result:= FHeight;
end;

function TBGRALayeredBitmap.GetNbLayers: integer;
begin
  Result:= FNbLayers;
end;

function TBGRALayeredBitmap.GetBlendOperation(Layer: integer): TBlendOperation;
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
    Result:= FLayers[layer].blendOp;
end;

function TBGRALayeredBitmap.GetLayerVisible(layer: integer): boolean;
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
    Result:= FLayers[layer].Visible;
end;

function TBGRALayeredBitmap.GetLayerOpacity(layer: integer): byte;
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
    Result:= FLayers[layer].Opacity;
end;

function TBGRALayeredBitmap.GetLayerOffset(layer: integer): TPoint;
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
    with FLayers[layer] do
      Result:= Point(x,y);
end;

function TBGRALayeredBitmap.GetLayerName(layer: integer): string;
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
  begin
    if not FLayers[layer].Owner and (FLayers[layer].Source <> nil) then
      Result := FLayers[layer].Source.Caption
    else
      Result:= FLayers[layer].Name;
    if Result = '' then
      result := inherited GetLayerName(layer);
  end;
end;

function TBGRALayeredBitmap.GetLayerFrozen(layer: integer): boolean;
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
    Result:= FLayers[layer].Frozen;
end;

procedure TBGRALayeredBitmap.SetBlendOperation(Layer: integer;
  op: TBlendOperation);
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
  begin
    if FLayers[layer].blendOp <> op then
    begin
      FLayers[layer].blendOp := op;
      Unfreeze(layer);
    end;
  end;
end;

procedure TBGRALayeredBitmap.SetLayerVisible(layer: integer; AValue: boolean);
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
  begin
    if FLayers[layer].Visible <> AValue then
    begin
      FLayers[layer].Visible := AValue;
      Unfreeze(layer);
    end;
  end;
end;

procedure TBGRALayeredBitmap.SetLayerOpacity(layer: integer; AValue: byte);
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
  begin
    if FLayers[layer].Opacity <> AValue then
    begin
      FLayers[layer].Opacity := AValue;
      Unfreeze(layer);
    end;
  end;
end;

procedure TBGRALayeredBitmap.SetLayerOffset(layer: integer; AValue: TPoint);
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
  begin
    if (FLayers[layer].x <> AValue.x) or
      (FLayers[layer].y <> AValue.y) then
    begin
      if FLayers[layer].OriginalGuid <> GUID_NULL then
        raise exception.Create('The offset of the layer is computed from an original. You can change it by changing the layer original matrix.');

      FLayers[layer].x := AValue.x;
      FLayers[layer].y := AValue.y;
      Unfreeze(layer);
    end;
  end;
end;

procedure TBGRALayeredBitmap.SetLayerName(layer: integer; AValue: string);
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
  begin
    if not FLayers[layer].Owner and (FLayers[layer].Source <> nil) then
      FLayers[layer].Source.Caption := AValue
    else
      FLayers[layer].Name := AValue;
  end;
end;

procedure TBGRALayeredBitmap.SetLayerFrozen(layer: integer; AValue: boolean);
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
    FLayers[layer].Frozen := AValue;
end;

function TBGRALayeredBitmap.GetLayerBitmapDirectly(layer: integer): TBGRABitmap;
begin
  if (layer < 0) or (layer >= NbLayers) then
    result := nil
  else
  begin
    if FLayers[layer].OriginalRenderStatus = orsNone then
      RenderLayerFromOriginal(layer, true)
    else if FLayers[layer].OriginalRenderStatus in [orsPartialDraft,orsPartialProof] then
      RenderLayerFromOriginal(layer, true, FLayers[layer].OriginalInvalidatedBounds);
    Result:= FLayers[layer].Source;
  end;
end;

procedure TBGRALayeredBitmap.LoadFromFile(const filenameUTF8: string);
var bmp: TBGRABitmap;
    ext: string;
    temp: TBGRACustomLayeredBitmap;
    i: integer;
    stream: TFileStreamUTF8;
begin
  ext := UTF8LowerCase(ExtractFileExt(filenameUTF8));
  for i := 0 to high(LayeredBitmapReaders) do
    if '.'+LayeredBitmapReaders[i].extension = ext then
    begin
      temp := LayeredBitmapReaders[i].theClass.Create;
      try
        temp.LoadFromFile(filenameUTF8);
        Assign(temp);
      finally
        temp.Free;
      end;
      exit;
    end;

  //when using "data" extension, simply deserialize
  if (ext='.dat') or (ext='.data') then
  begin
    if Assigned(LayeredBitmapLoadFromStreamProc) then
    begin
      stream := TFileStreamUTF8.Create(filenameUTF8, fmOpenRead, fmShareDenyWrite);
      try
        LayeredBitmapLoadFromStreamProc(stream, self);
      finally
        stream.Free;
      end;
    end else
      raise exception.Create('Enable layer deserialization by calling BGRAStreamLayers.RegisterStreamLayers');
  end else
  begin
    bmp := TBGRABitmap.Create(filenameUTF8, True);
    Clear;
    SetSize(bmp.Width,bmp.Height);
    AddOwnedLayer(bmp);
  end;
end;

procedure TBGRALayeredBitmap.LoadFromStream(stream: TStream);
var bmp: TBGRABitmap;
begin
  if Assigned(LayeredBitmapLoadFromStreamProc) then
  begin
    if not Assigned(LayeredBitmapCheckStreamProc) or
      LayeredBitmapCheckStreamProc(stream) then
    begin
      LayeredBitmapLoadFromStreamProc(Stream, self);
      exit;
    end;
  end;

  bmp := TBGRABitmap.Create(stream);
  Clear;
  SetSize(bmp.Width,bmp.Height);
  AddOwnedLayer(bmp);
end;

procedure TBGRALayeredBitmap.LoadFromResource(AFilename: string);
var
  stream: TStream;
begin
  stream := BGRAResource.GetResourceStream(AFilename);
  try
    LoadFromStream(stream);
  finally
    stream.Free;
  end;
end;

procedure TBGRALayeredBitmap.SetSize(AWidth, AHeight: integer);
begin
  Unfreeze;
  FWidth := AWidth;
  FHeight := AHeight;
end;

procedure TBGRALayeredBitmap.Clear;
var i: integer;
begin
  Unfreeze;
  for i := NbLayers-1 downto 0 do
    RemoveLayer(i);
  MemDirectory := nil;
  ClearOriginals;
end;

procedure TBGRALayeredBitmap.ClearOriginals;
var
  i: Integer;
begin
  if Assigned(FOriginals) then
  begin
    for i := 0 to OriginalCount-1 do
      FOriginals[i].Instance.Free;
    FreeAndNil(FOriginals);
  end;
end;

procedure TBGRALayeredBitmap.RemoveLayer(index: integer);
var i: integer;
  id: LongInt;
  layersDir: TMemDirectory;
begin
  if (index < 0) or (index >= NbLayers) then exit;
  Unfreeze;
  if Assigned(FMemDirectory) then
  begin
    id := LayerUniqueId[index];
    if FMemDirectory.IndexOf(LayersDirectory,'')<>-1 then
    begin
      layersDir := FMemDirectory.Directory[FMemDirectory.AddDirectory(LayersDirectory)];
      layersDir.Delete(IntToStr(id),'');
    end;
  end;
  if FLayers[index].Owner then FLayers[index].Source.Free;
  for i := index to FNbLayers-2 do
    FLayers[i] := FLayers[i+1];
  Dec(FNbLayers);
end;

procedure TBGRALayeredBitmap.InsertLayer(index: integer; fromIndex: integer);
var info: TBGRALayerInfo;
    i: integer;
begin
  if (index < 0) or (index > NbLayers) or (index = fromIndex) then exit;
  if (fromIndex < 0) or (fromIndex >= NbLayers) then exit;
  Unfreeze;
  info := FLayers[fromIndex];
  for i := fromIndex to FNbLayers-2 do
    FLayers[i] := FLayers[i+1];
  for i := FNbLayers-1 downto index+1 do
    FLayers[i] := FLayers[i-1];
  FLayers[index] := info;
end;

procedure TBGRALayeredBitmap.Assign(ASource: TBGRACustomLayeredBitmap; ASharedLayerIds: boolean;
  ACopyAdditionalMemData: boolean);
var i,idx,idxOrig,idxNewOrig: integer;
    usedOriginals: array of record
       used: boolean;
       sourceGuid,newGuid: TGuid;
    end;
    orig: TBGRALayerCustomOriginal;
    stream: TMemoryStream;
    targetDir, layerDir: TMemDirectory;

begin
  if ASource = nil then
    raise exception.Create('Unexpected nil reference');
  Clear;
  SetSize(ASource.Width,ASource.Height);
  LinearBlend:= ASource.LinearBlend;
  setlength(usedOriginals, ASource.GetOriginalCount);
  for idxOrig := 0 to high(usedOriginals) do
  with usedOriginals[idxOrig] do
  begin
    used:= false;
    newGuid := GUID_NULL;
  end;
  for i := 0 to ASource.NbLayers-1 do
  if (ASource.LayerOriginalGuid[i]<>GUID_NULL) and
     (ASource.LayerOriginalKnown[i] or (ASource is TBGRALayeredBitmap)) then
  begin
    idxOrig := ASource.IndexOfOriginal(ASource.LayerOriginalGuid[i]);
    if (idxOrig <> -1) and not usedOriginals[idxOrig].used then
    begin
      if ASource.GetOriginalByIndexLoaded(idxOrig) then
      begin
        orig := ASource.GetOriginalByIndex(idxOrig);
        idxNewOrig := AddOriginal(orig, false);
        usedOriginals[idxOrig].sourceGuid := orig.Guid;
      end else
      begin
        stream := TMemoryStream.Create;
        (ASource as TBGRALayeredBitmap).SaveOriginalToStream(idxOrig, stream);
        stream.Position:= 0;
        idxNewOrig := AddOriginalFromStream(stream,true);
        stream.Free;
        usedOriginals[idxOrig].sourceGuid := (ASource as TBGRALayeredBitmap).OriginalGuid[idxOrig];
      end;
      usedOriginals[idxOrig].newGuid := OriginalGuid[idxNewOrig];
      usedOriginals[idxOrig].used := true;
    end;
  end;
  for i := 0 to ASource.NbLayers-1 do
  begin
    idx := AddOwnedLayer(ASource.GetLayerBitmapCopy(i),ASource.LayerOffset[i],ASource.BlendOperation[i],ASource.LayerOpacity[i]);
    LayerName[idx] := ASource.LayerName[i];
    LayerVisible[idx] := ASource.LayerVisible[i];
    if ASharedLayerIds and (ASource is TBGRALayeredBitmap) then
      LayerUniqueId[idx] := TBGRALayeredBitmap(ASource).LayerUniqueId[i];
    for idxOrig := 0 to high(usedOriginals) do
      if usedOriginals[idxOrig].sourceGuid = ASource.LayerOriginalGuid[i] then
      begin
        LayerOriginalGuid[idx] := usedOriginals[idxOrig].newGuid;
        LayerOriginalMatrix[idx] := ASource.LayerOriginalMatrix[i];
        LayerOriginalRenderStatus[idx] := ASource.LayerOriginalRenderStatus[i];
        break;
      end;
    if ASource is TBGRALayeredBitmap then
    begin
      layerDir := TBGRALayeredBitmap(ASource).GetLayerDirectory(i,false);
      if Assigned(layerDir) then
        layerDir.CopyTo(GetLayerDirectory(idx,true), true);
    end;
  end;
  if ACopyAdditionalMemData and ASource.HasMemFiles then
    for i := 0 to ASource.GetMemDirectory.Count-1 do
    if (ASource.GetMemDirectory.Entry[i].CompareNameAndExtension(OriginalsDirectory,'')<>0) and
       (ASource.GetMemDirectory.Entry[i].CompareNameAndExtension(LayersDirectory,'')<>0) and
       (ASource.GetMemDirectory.IsDirectory[i]) then
    begin
      with ASource.GetMemDirectory.Entry[i] do
        targetDir := GetMemDirectory.Directory[GetMemDirectory.AddDirectory(Name,Extension)];
      ASource.GetMemDirectory.Directory[i].CopyTo(targetDir, true);
    end;
end;

function TBGRALayeredBitmap.MoveLayerUp(index: integer): integer;
begin
  if (index >= 0) and (index <= NbLayers-2) then
  begin
    InsertLayer(index+1,index);
    result := index+1;
  end else
    result := -1;
end;

function TBGRALayeredBitmap.MoveLayerDown(index: integer): integer;
begin
  if (index > 0) and (index <= NbLayers-1) then
  begin
    InsertLayer(index-1,index);
    result := index-1;
  end else
    result := -1;
end;

function TBGRALayeredBitmap.AddLayer(Source: TBGRABitmap; Opacity: byte
  ): integer;
begin
  result := AddLayer(Source, Point(0,0), DefaultBlendingOperation, Opacity, False);
end;

function TBGRALayeredBitmap.AddLayer(Source: TBGRABitmap; Position: TPoint;
  BlendOp: TBlendOperation; Opacity: byte; Shared: boolean): integer;
begin
  result := AddLayer(Source.Caption,Source,Position,BlendOp,Opacity,Shared);
end;

function TBGRALayeredBitmap.AddLayer(Source: TBGRABitmap; Position: TPoint;
  Opacity: byte): integer;
begin
  result := AddLayer(Source,Position,DefaultBlendingOperation,Opacity);
end;

function TBGRALayeredBitmap.AddLayer(Source: TBGRABitmap;
  BlendOp: TBlendOperation; Opacity: byte): integer;
begin
  result := AddLayer(Source,Point(0,0),BlendOp,Opacity);
end;

function TBGRALayeredBitmap.AddLayer(AName: string; Source: TBGRABitmap;
  Opacity: byte): integer;
begin
  result := AddLayer(AName,Source,Point(0,0),Opacity);
end;

function TBGRALayeredBitmap.AddLayer(AName: string; Source: TBGRABitmap;
  Position: TPoint; BlendOp: TBlendOperation; Opacity: byte; Shared: boolean): integer;
begin
  if length(FLayers) = FNbLayers then
    setlength(FLayers, length(FLayers)*2+1);
  FLayers[FNbLayers].Name := AName;
  FLayers[FNbLayers].X := Position.X;
  FLayers[FNbLayers].Y := Position.Y;
  FLayers[FNbLayers].blendOp := BlendOp;
  FLayers[FNbLayers].Opacity := Opacity;
  FLayers[FNbLayers].Visible := true;
  FLayers[FNbLayers].Frozen := false;
  FLayers[FNbLayers].UniqueId := ProduceLayerUniqueId;
  FLayers[FNbLayers].OriginalMatrix := AffineMatrixIdentity;
  FLayers[FNbLayers].OriginalRenderStatus := orsNone;
  FLayers[FNbLayers].OriginalGuid := GUID_NULL;
  if Shared then
  begin
    FLayers[FNbLayers].Source := Source;
    FLayers[FNbLayers].Owner := false;
  end else
  begin
    FLayers[FNbLayers].Source := Source.Duplicate;
    FLayers[FNbLayers].Owner := true;
  end;
  result := FNbLayers;
  inc(FNbLayers);
  if (FNbLayers = 1) and (FWidth = 0) and (FHeight = 0) and (Source <> nil) then
    SetSize(Source.Width,Source.Height);
end;

function TBGRALayeredBitmap.AddLayer(AName: string; Source: TBGRABitmap;
  Position: TPoint; Opacity: byte): integer;
begin
  result := AddLayer(AName, Source, Position, DefaultBlendingOperation, Opacity);
end;

function TBGRALayeredBitmap.AddLayer(AName: string; Source: TBGRABitmap;
  BlendOp: TBlendOperation; Opacity: byte): integer;
begin
  result := AddLayer(AName, Source, Point(0,0), blendOp, Opacity);
end;

function TBGRALayeredBitmap.AddSharedLayer(Source: TBGRABitmap; Opacity: byte
  ): integer;
begin
  result := AddSharedLayer(Source, Point(0,0), DefaultBlendingOperation, Opacity);
end;

function TBGRALayeredBitmap.AddSharedLayer(Source: TBGRABitmap;
  Position: TPoint; BlendOp: TBlendOperation; Opacity: byte): integer;
begin
  result := AddLayer(Source, Position, BlendOp, Opacity, True);
end;

function TBGRALayeredBitmap.AddSharedLayer(Source: TBGRABitmap;
  Position: TPoint; Opacity: byte): integer;
begin
  result := AddSharedLayer(Source, Position, DefaultBlendingOperation, Opacity);
end;

function TBGRALayeredBitmap.AddSharedLayer(Source: TBGRABitmap;
  BlendOp: TBlendOperation; Opacity: byte): integer;
begin
  result := AddSharedLayer(Source, Point(0,0), blendOp, Opacity);
end;

function TBGRALayeredBitmap.AddLayerFromFile(AFileName: string; Opacity: byte
  ): integer;
begin
  result := AddOwnedLayer(TBGRABitmap.Create(AFilename),Opacity);
  FLayers[result].Name := ExtractFileName(AFilename);
end;

function TBGRALayeredBitmap.AddLayerFromFile(AFileName: string;
  Position: TPoint; BlendOp: TBlendOperation; Opacity: byte): integer;
begin
  result := AddOwnedLayer(TBGRABitmap.Create(AFilename),Position,BlendOp,Opacity);
  FLayers[result].Name := ExtractFileName(AFilename);
end;

function TBGRALayeredBitmap.AddLayerFromFile(AFileName: string;
  Position: TPoint; Opacity: byte): integer;
begin
  result := AddOwnedLayer(TBGRABitmap.Create(AFilename),Position,Opacity);
  FLayers[result].Name := ExtractFileName(AFilename);
end;

function TBGRALayeredBitmap.AddLayerFromFile(AFileName: string;
  BlendOp: TBlendOperation; Opacity: byte): integer;
begin
  result := AddOwnedLayer(TBGRABitmap.Create(AFilename),BlendOp,Opacity);
  FLayers[result].Name := ExtractFileName(AFilename);
end;

function TBGRALayeredBitmap.AddOwnedLayer(ABitmap: TBGRABitmap; Opacity: byte
  ): integer;
begin
  result := AddSharedLayer(ABitmap,Opacity);
  FLayers[result].Owner := True;
end;

function TBGRALayeredBitmap.AddOwnedLayer(ABitmap: TBGRABitmap;
  Position: TPoint; BlendOp: TBlendOperation; Opacity: byte): integer;
begin
  result := AddSharedLayer(ABitmap,Position,BlendOp,Opacity);
  FLayers[result].Owner := True;
end;

function TBGRALayeredBitmap.AddOwnedLayer(ABitmap: TBGRABitmap;
  Position: TPoint; Opacity: byte): integer;
begin
  result := AddSharedLayer(ABitmap,Position,Opacity);
  FLayers[result].Owner := True;
end;

function TBGRALayeredBitmap.AddOwnedLayer(ABitmap: TBGRABitmap;
  BlendOp: TBlendOperation; Opacity: byte): integer;
begin
  result := AddSharedLayer(ABitmap,BlendOp,Opacity);
  FLayers[result].Owner := True;
end;

function TBGRALayeredBitmap.AddLayerFromOriginal(const AGuid: TGuid;
  Opacity: byte): integer;
begin
  result := AddLayerFromOriginal(AGuid, DefaultBlendingOperation, Opacity);
end;

function TBGRALayeredBitmap.AddLayerFromOriginal(const AGuid: TGuid;
  BlendOp: TBlendOperation; Opacity: byte): integer;
begin
  result := AddLayerFromOriginal(AGuid, AffineMatrixIdentity, BlendOp, Opacity);
end;

function TBGRALayeredBitmap.AddLayerFromOriginal(const AGuid: TGuid;
  Matrix: TAffineMatrix; Opacity: byte): integer;
begin
  result := AddLayerFromOriginal(AGuid, Matrix, DefaultBlendingOperation, Opacity);
end;

function TBGRALayeredBitmap.AddLayerFromOriginal(const AGuid: TGuid;
  Matrix: TAffineMatrix; BlendOp: TBlendOperation; Opacity: byte): integer;
begin
  result := AddOwnedLayer(TBGRABitmap.Create, BlendOp, Opacity);
  LayerOriginalGuid[result] := AGuid;
  LayerOriginalMatrix[result] := Matrix;
  if not Assigned(LayerOriginal[result]) then
    raise exception.Create('Original data or class not found');
end;

function TBGRALayeredBitmap.AddLayerFromOwnedOriginal(
  AOriginal: TBGRALayerCustomOriginal; Opacity: byte): integer;
begin
  if IndexOfOriginal(AOriginal) = -1 then AddOriginal(AOriginal);
  result := AddLayerFromOriginal(AOriginal.Guid, Opacity);
end;

function TBGRALayeredBitmap.AddLayerFromOwnedOriginal(
  AOriginal: TBGRALayerCustomOriginal; BlendOp: TBlendOperation; Opacity: byte): integer;
begin
  if IndexOfOriginal(AOriginal) = -1 then AddOriginal(AOriginal);
  result := AddLayerFromOriginal(AOriginal.Guid, BlendOp, Opacity);
end;

function TBGRALayeredBitmap.AddLayerFromOwnedOriginal(
  AOriginal: TBGRALayerCustomOriginal; Matrix: TAffineMatrix; Opacity: byte): integer;
begin
  if IndexOfOriginal(AOriginal) = -1 then AddOriginal(AOriginal);
  result := AddLayerFromOriginal(AOriginal.Guid, Matrix, Opacity);
end;

function TBGRALayeredBitmap.AddLayerFromOwnedOriginal(
  AOriginal: TBGRALayerCustomOriginal; Matrix: TAffineMatrix;
  BlendOp: TBlendOperation; Opacity: byte): integer;
begin
  if IndexOfOriginal(AOriginal) = -1 then AddOriginal(AOriginal);
  result := AddLayerFromOriginal(AOriginal.Guid, Matrix, BlendOp, Opacity);
end;

class function TBGRALayeredBitmap.IsValidRegistryIndentifier(AIdentifier: string): boolean;
var
  i: Integer;
begin
  if length(AIdentifier) = 0 then exit(false);
  for i := 1 to length(AIdentifier) do
    if not (AIdentifier[i] in ['A'..'Z','a'..'z','0'..'9','_','-']) then exit(false);
  exit(true);
end;

function TBGRALayeredBitmap.GetLayerRegistry(ALayerIndex: integer;
  ARegistryIdentifier: string): RawByteString;
var
  layerDir, registryDir: TMemDirectory;
begin
  if not IsValidRegistryIndentifier(ARegistryIdentifier) then
    raise exception.Create('Invalid registry identifier');
  layerDir := GetLayerDirectory(ALayerIndex, false);
  if layerDir = nil then exit('');
  registryDir := layerDir.Directory[layerDir.AddDirectory(RegistrySubDirectory,'')];
  result := registryDir.RawStringByFilename[ARegistryIdentifier]
end;

procedure TBGRALayeredBitmap.SetLayerRegistry(ALayerIndex: integer;
  ARegistryIdentifier: string; AValue: RawByteString);
var
  layerDir, registryDir: TMemDirectory;
begin
  if not IsValidRegistryIndentifier(ARegistryIdentifier) then
    raise exception.Create('Invalid registry identifier');
  layerDir := GetLayerDirectory(ALayerIndex, true);
  registryDir := layerDir.Directory[layerDir.AddDirectory(RegistrySubDirectory,'')];
  if length(AValue) = 0 then
    registryDir.Delete(ARegistryIdentifier,'')
  else registryDir.RawStringByFilename[ARegistryIdentifier] := AValue;
end;

procedure TBGRALayeredBitmap.SaveLayerRegistryToStream(ALayerIndex: integer;
  AStream: TStream);
var
  layerDir, registryDir: TMemDirectory;
begin
  layerDir := GetLayerDirectory(ALayerIndex, true);
  registryDir := layerDir.Directory[layerDir.AddDirectory(RegistrySubDirectory,'')];
  registryDir.SaveToStream(AStream);
end;

procedure TBGRALayeredBitmap.LoadLayerRegistryFromStream(ALayerIndex: integer;
  AStream: TStream);
var
  layerDir, registryDir: TMemDirectory;
begin
  layerDir := GetLayerDirectory(ALayerIndex, true);
  registryDir := layerDir.Directory[layerDir.AddDirectory(RegistrySubDirectory,'')];
  registryDir.LoadFromStream(AStream);
end;

function TBGRALayeredBitmap.GetGlobalRegistry(ARegistryIdentifier: string): RawByteString;
var
  registryDir: TMemDirectory;
begin
  if not IsValidRegistryIndentifier(ARegistryIdentifier) then
    raise exception.Create('Invalid registry identifier');
  registryDir := MemDirectory.Directory[MemDirectory.AddDirectory(RegistrySubDirectory,'')];
  result := registryDir.RawStringByFilename[ARegistryIdentifier]
end;

procedure TBGRALayeredBitmap.SetGlobalRegistry(ARegistryIdentifier: string; AValue: RawByteString);
var
  registryDir: TMemDirectory;
begin
  if not IsValidRegistryIndentifier(ARegistryIdentifier) then
    raise exception.Create('Invalid registry identifier');
  registryDir := MemDirectory.Directory[MemDirectory.AddDirectory(RegistrySubDirectory,'')];
  if length(AValue) = 0 then
    registryDir.Delete(ARegistryIdentifier,'')
  else registryDir.RawStringByFilename[ARegistryIdentifier] := AValue;
end;

function TBGRALayeredBitmap.AddOriginal(AOriginal: TBGRALayerCustomOriginal; AOwned: boolean): integer;
var
  newGuid: TGuid;
begin
  if AOriginal = nil then
    raise exception.Create('Unexpected nil reference');;
  if AOriginal.Guid = GUID_NULL then
  begin
    if CreateGUID(newGuid)<> 0 then
    begin
      if AOwned then AOriginal.Free;
      raise exception.Create('Error while creating GUID');
    end;
    AOriginal.Guid := newGuid;
  end else
  begin
    if IndexOfOriginal(AOriginal) <> -1 then
    begin
      if AOwned then AOriginal.Free;
      raise exception.Create('Original already added');
    end;
    if IndexOfOriginal(AOriginal.Guid) <> -1 then
    begin
      if AOwned then AOriginal.Free;
      raise exception.Create('GUID is already in use');
    end;
  end;
  if FOriginals = nil then FOriginals := TBGRALayerOriginalList.Create;
  if AOwned then
  begin
    result := FOriginals.Add(BGRALayerOriginalEntry(AOriginal));
    AOriginal.OnChange:= @OriginalChange;
    AOriginal.OnEditingChange:= @OriginalEditingChange;
  end
  else
  begin
    StoreOriginal(AOriginal);
    result := FOriginals.Add(BGRALayerOriginalEntry(AOriginal.Guid));
  end;
end;

function TBGRALayeredBitmap.AddOriginalFromStream(AStream: TStream;
  ALateLoad: boolean): integer;
var
  newGuid: TGUID;
begin
  if CreateGUID(newGuid)<> 0 then raise exception.Create('Error while creating GUID');
  result := AddOriginalFromStream(AStream, newGuid, ALateLoad);
end;


function TBGRALayeredBitmap.AddOriginalFromStream(AStream: TStream;
  const AGuid: TGuid; ALateLoad: boolean): integer;
var
  storage: TBGRAMemOriginalStorage;
begin
  storage:= TBGRAMemOriginalStorage.Create;
  storage.LoadFromStream(AStream);
  try
    result := AddOriginalFromStorage(storage, AGuid, ALateLoad);
  finally
    storage.Free;
  end;
end;

function TBGRALayeredBitmap.AddOriginalFromStorage(AStorage: TBGRAMemOriginalStorage; ALateLoad: boolean): integer;
var
  newGuid: TGUID;
begin
  if CreateGUID(newGuid)<> 0 then raise exception.Create('Error while creating GUID');
  result := AddOriginalFromStorage(AStorage, newGuid, ALateLoad);
end;

function TBGRALayeredBitmap.AddOriginalFromStorage(
  AStorage: TBGRAMemOriginalStorage; const AGuid: TGuid; ALateLoad: boolean): integer;
var
  origClassName: String;
  origClass: TBGRALayerOriginalAny;
  orig: TBGRALayerCustomOriginal;
  dir, subdir: TMemDirectory;
  raiseError: Boolean;
begin
  result := -1;
  origClassName := AStorage.RawString['class'];
  if origClassName = '' then raise Exception.Create('Original class name not defined');
  if ALateLoad then
  begin
    if IndexOfOriginal(AGuid)<>-1 then
      raise exception.Create('Duplicate GUID');

    dir := MemDirectory.Directory[MemDirectory.AddDirectory(OriginalsDirectory)];
    subdir := dir.Directory[dir.AddDirectory(GUIDToString(AGuid))];
    AStorage.CopyTo(subdir);

    if FOriginals = nil then FOriginals := TBGRALayerOriginalList.Create;
    result := FOriginals.Add(BGRALayerOriginalEntry(AGuid));
  end else
  begin
    origClass := FindLayerOriginalClass(origClassName);
    if origClass = nil then raise exception.Create('Original class not found (it can be registered with the RegisterLayerOriginal function)');
    orig := origClass.Create;
    try
      orig.LoadFromStorage(AStorage);
      orig.Guid := AGuid;
      result := AddOriginal(orig, true);
    except on ex:exception do
      begin
        orig.Free;
        raiseError := true;
        if Assigned(FOnOriginalLoadError) then
          FOnOriginalLoadError(self, ex.Message, raiseError);
        if raiseError then
          raise ex;
      end;
    end;
  end;
end;

procedure TBGRALayeredBitmap.SaveOriginalToStream(AIndex: integer;
  AStream: TStream);
var
  dir: TMemDirectory;
  c: TBGRALayerOriginalAny;
begin
  if (AIndex < 0) or (AIndex >= OriginalCount) then
    raise ERangeError.Create('Index out of bounds');

  if Assigned(FOriginals[AIndex].Instance) then
    FOriginals[AIndex].Instance.SaveToStream(AStream)
  else
  begin
    FindOriginal(FOriginals[AIndex].Guid, dir, c);
    if dir = nil then
      raise exception.Create('Original directory not found');
    dir.SaveToStream(AStream);
  end;
end;

procedure TBGRALayeredBitmap.SaveOriginalToStream(const AGuid: TGuid;
  AStream: TStream);
var
  idxOrig: Integer;
begin
  idxOrig := IndexOfOriginal(AGuid);
  if idxOrig = -1 then raise exception.Create('Original not found');
  SaveOriginalToStream(idxOrig, AStream);
end;

function TBGRALayeredBitmap.RemoveOriginal(AOriginal: TBGRALayerCustomOriginal): boolean;
var
  idx: Integer;
begin
  idx := IndexOfOriginal(AOriginal);
  if idx = -1 then exit(false);
  DeleteOriginal(idx);
  result := true;
end;

procedure TBGRALayeredBitmap.DeleteOriginal(AIndex: integer);
var
  dir: TMemDirectory;
  i: Integer;
  guid: TGuid;
begin
  if (AIndex < 0) or (AIndex >= OriginalCount) then
    raise ERangeError.Create('Index out of bounds');

  guid := FOriginals[AIndex].Guid;
  for i := 0 to NbLayers-1 do
    if LayerOriginalGuid[i] = guid then
    begin
      LayerOriginalGuid[i] := GUID_NULL;
      LayerOriginalMatrix[i] := AffineMatrixIdentity;
    end;

  dir := MemDirectory.Directory[MemDirectory.AddDirectory(OriginalsDirectory)];
  dir.Delete(GUIDToString(guid),'');

  FOriginals[AIndex].Instance.Free;
  FOriginals.Delete(AIndex); //AOriginals freed
end;

procedure TBGRALayeredBitmap.NotifyLoaded;
var
  foundGuid: array of TGuid;
  nbFoundGuid: integer;

  procedure AddGuid(const AGuid: TGuid);
  begin
    foundGuid[nbFoundGuid] := AGuid;
    inc(nbFoundGuid);
  end;

  function IndexOfGuid(AGuid: TGuid): integer;
  var
    i: Integer;
  begin
    for i := 0 to nbFoundGuid-1 do
      if foundGuid[i] = AGuid then exit(i);
    result := -1;
  end;

var
  i: Integer;
  dir: TMemDirectory;
  newGuid: TGUID;

begin
  inherited NotifyLoaded;

  //if there are no files in memory, we are sure that there are no originals
  if not HasMemFiles then
  begin
    ClearOriginals;
    exit;
  end;

  //determine list of GUID of originals
  dir := MemDirectory.Directory[MemDirectory.AddDirectory(OriginalsDirectory)];
  setlength(foundGuid, dir.Count);
  nbFoundGuid:= 0;
  for i := 0 to dir.Count-1 do
    if dir.IsDirectory[i] and (dir.Entry[i].Extension = '') then
    begin
      if TryStringToGUID(dir.Entry[i].Name, newGuid) then
        AddGuid(newGuid);
    end;

  //remove originals that do not exist anymore
  for i := OriginalCount-1 downto 0 do
    if IndexOfGuid(FOriginals[i].Guid) = -1 then
      DeleteOriginal(i);

  //add originals from memory directory
  for i := 0 to nbFoundGuid-1 do
  begin
    if IndexOfOriginal(foundGuid[i]) = -1 then
    begin
      if FOriginals = nil then FOriginals := TBGRALayerOriginalList.Create;
      FOriginals.Add(BGRALayerOriginalEntry(foundGuid[i]));
    end;
  end;
end;

procedure TBGRALayeredBitmap.NotifySaving;
var
  i, j, id, ErrPos: Integer;
  layersDir, renderDir: TMemDirectory;
begin
  inherited NotifySaving;

  RenderOriginalsIfNecessary;

  for i := 0 to OriginalCount-1 do
    if Assigned(FOriginals[i].Instance) then
      StoreOriginal(FOriginals[i].Instance);

  //check layer storage
  if MemDirectory.IndexOf(LayersDirectory,'')<>-1 then
  begin
    layersDir := MemDirectory.Directory[MemDirectory.AddDirectory(LayersDirectory)];
    for i := layersDir.Count-1 downto 0 do
    if layersDir.IsDirectory[i] then
    begin
      renderDir := layersDir.Directory[i].FindPath(RenderSubDirectory);

      if Assigned(renderDir) then
      begin
        //discard temporary files
        renderDir.Delete(RenderTempSubDirectory,'');

        //compress significant files
        for j := 0 to renderDir.Count-1 do
        begin
          if renderDir.Entry[j].FileSize > 128 then
            renderDir.IsEntryCompressed[j] := true;
        end;
      end;

      //remove invalid layer references
      val(layersDir.Entry[i].Name, id, errPos);
      if (errPos <> 0) or (GetLayerIndexFromId(id)=-1) then
        layersDir.Delete(i);
    end;
    if layersDir.Count = 0 then
      MemDirectory.Delete(LayersDirectory,'');
  end;
end;

procedure TBGRALayeredBitmap.RenderLayerFromOriginal(layer: integer;
  ADraft: boolean; AFullSizeLayer: boolean = false);
begin
  RenderLayerFromOriginal(layer, ADraft, rectF(0,0,Width,Height), AFullSizeLayer);
end;

procedure TBGRALayeredBitmap.RenderLayerFromOriginal(layer: integer;
  ADraft: boolean; ARenderBounds: TRect; AFullSizeLayer: boolean = false);
var
  orig: TBGRALayerCustomOriginal;
  rAll, rNewBounds, rInterRender: TRect;
  newSource: TBGRABitmap;
  layerDir, renderDir: TMemDirectory;
  j: integer;

  procedure FreeSource;
  begin
    if FLayers[layer].Owner then
      FreeAndNil(FLayers[layer].Source)
    else
      FLayers[layer].Source := nil;
  end;

begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds');

  orig := LayerOriginal[layer];
  if Assigned(orig) then
  begin
    Unfreeze(layer);
    layerDir := GetLayerDirectory(layer, true);
    renderDir := layerDir.Directory[layerDir.AddDirectory(RenderSubDirectory)];
    //uncompress files for faster access
    for j := 0 to renderDir.Count-1 do
      renderDir.IsEntryCompressed[j] := false;
    orig.RenderStorage := TBGRAMemOriginalStorage.Create(renderDir);

    rAll := rect(0,0,Width,Height);
    if AFullSizeLayer then
      rNewBounds := rAll
    else
    begin
      rNewBounds := orig.GetRenderBounds(rAll,FLayers[layer].OriginalMatrix);
      rNewBounds.Intersect(rAll);
    end;
    rInterRender := TRect.Intersect(ARenderBounds, rNewBounds);
    if (FLayers[layer].x = rNewBounds.Left) and
      (FLayers[layer].y = rNewBounds.Top) and
      Assigned(FLayers[layer].Source) and
      (FLayers[layer].Source.Width = rNewBounds.Width) and
      (FLayers[layer].Source.Height = rNewBounds.Height) then
    begin
      rInterRender.Offset(-rNewBounds.Left, -rNewBounds.Top);
      FLayers[layer].Source.FillRect(rInterRender, BGRAPixelTransparent, dmSet);
      FLayers[layer].Source.ClipRect := rInterRender;
      orig.Render(FLayers[layer].Source, Point(-rNewBounds.Left,-rNewBounds.Top), FLayers[layer].OriginalMatrix, ADraft);
      FLayers[layer].Source.NoClip;
    end else
    begin
      if rInterRender = rNewBounds then
      begin
        FreeSource;
        newSource := TBGRABitmap.Create(rNewBounds.Width,rNewBounds.Height);
        orig.Render(newSource, Point(-rNewBounds.Left,-rNewBounds.Top), FLayers[layer].OriginalMatrix, ADraft);
      end else
      begin
        newSource := TBGRABitmap.Create(rNewBounds.Width,rNewBounds.Height);
        newSource.PutImage(FLayers[layer].x - rNewBounds.Left, FLayers[layer].y - rNewBounds.Top, FLayers[layer].Source, dmSet);
        FreeSource;
        rInterRender.Offset(-rNewBounds.Left, -rNewBounds.Top);
        if not rInterRender.IsEmpty then
        begin
          newSource.FillRect(rInterRender, BGRAPixelTransparent, dmSet);
          newSource.ClipRect := rInterRender;
          orig.Render(newSource, Point(-rNewBounds.Left,-rNewBounds.Top), FLayers[layer].OriginalMatrix, ADraft);
          newSource.NoClip;
        end;
      end;
      FLayers[layer].Source := newSource;
      FLayers[layer].x := rNewBounds.Left;
      FLayers[layer].y := rNewBounds.Top;
    end;

    orig.RenderStorage.AffineMatrix['last-matrix'] := FLayers[layer].OriginalMatrix;
    orig.RenderStorage.Free;
    orig.renderStorage := nil;
    if renderDir.Count = 1 then //only matrix
      layerDir.Delete(RenderSubDirectory,'');
  end;
  if ADraft then
    FLayers[layer].OriginalRenderStatus := orsDraft
  else
    FLayers[layer].OriginalRenderStatus := orsProof;
  FLayers[layer].OriginalInvalidatedBounds := EmptyRectF;
end;

procedure TBGRALayeredBitmap.RenderLayerFromOriginal(layer: integer;
  ADraft: boolean; ARenderBoundsF: TRectF; AFullSizeLayer: boolean = false);
var
  r: TRect;
begin
  with ARenderBoundsF do
    r := Rect(floor(Left),floor(Top),ceil(Right),ceil(Bottom));
  RenderLayerFromOriginal(layer, ADraft, r, AFullSizeLayer);
end;

procedure TBGRALayeredBitmap.RenderLayerFromOriginalIfNecessary(layer: integer;
  ADraft: boolean; var ABounds: TRect);
  procedure UnionLayerArea(ALayer: integer);
  var
    r: TRect;
  begin
    if (FLayers[ALayer].Source = nil) or
      (FLayers[ALayer].Source.Width = 0) or
      (FLayers[ALayer].Source.Height = 0) then exit;

    r := RectWithSize(LayerOffset[ALayer].X, LayerOffset[ALayer].Y,
                      FLayers[ALayer].Source.Width, FLayers[ALayer].Source.Height);
    if ABounds.IsEmpty then ABounds := r else
      ABounds.Union(r);
  end;

var
  r: TRect;

begin
  case LayerOriginalRenderStatus[layer] of
  orsNone:
       begin
         UnionLayerArea(layer);
         RenderLayerFromOriginal(layer, ADraft);
         UnionLayerArea(layer);
       end;
  orsDraft: if not ADraft then
       begin
         UnionLayerArea(layer);
         RenderLayerFromOriginal(layer, ADraft);
         UnionLayerArea(layer);
       end;
  orsPartialDraft,orsPartialProof:
       if not ADraft and (LayerOriginalRenderStatus[layer] = orsPartialDraft) then
       begin
         UnionLayerArea(layer);
         RenderLayerFromOriginal(layer, ADraft, rect(0,0,Width,Height), true);
         UnionLayerArea(layer);
       end
       else
       begin
         with FLayers[layer].OriginalInvalidatedBounds do
           r := Rect(floor(Left),floor(Top),ceil(Right),ceil(Bottom));
         RenderLayerFromOriginal(layer, ADraft, r, true);
         if not r.Isempty then
         begin
           if ABounds.IsEmpty then
             ABounds := r
           else
             ABounds.Union(r);
         end;
       end;
  end;
end;

function TBGRALayeredBitmap.RenderOriginalsIfNecessary(ADraft: boolean): TRect;
var
  i: Integer;
begin
  result:= EmptyRect;
  for i := 0 to NbLayers-1 do
    RenderLayerFromOriginalIfNecessary(i, ADraft, result);
end;

function TBGRALayeredBitmap.RenderOriginalIfNecessary(const AGuid: TGuid;
  ADraft: boolean): TRect;
var
  i: Integer;
begin
  result:= EmptyRect;
  for i := 0 to NbLayers-1 do
    if LayerOriginalGuid[i] = AGuid then
      RenderLayerFromOriginalIfNecessary(i, ADraft, result);
end;

procedure TBGRALayeredBitmap.RemoveUnusedOriginals;
var useCount: array of integer;
  i, idxOrig: Integer;
begin
  if OriginalCount = 0 then exit;
  setlength(useCount, OriginalCount);
  for i := 0 to NbLayers-1 do
  begin
    idxOrig := IndexOfOriginal(LayerOriginalGuid[i]);
    if idxOrig <> -1 then inc(useCount[idxOrig]);
  end;
  for i := high(useCount) downto 0 do
    if useCount[i] = 0 then DeleteOriginal(i);
end;

procedure TBGRALayeredBitmap.UnloadOriginals;
var
  i: Integer;
begin
  for i := 0 to OriginalCount-1 do
    UnloadOriginal(i);
end;

procedure TBGRALayeredBitmap.UnloadOriginal(AIndex: integer);
var
  origInfo: TBGRALayerOriginalEntry;
begin
  if (AIndex >= 0) and (AIndex < OriginalCount) then
  begin
    origInfo := FOriginals[AIndex];
    if Assigned(origInfo.Instance) then
    begin
      StoreOriginal(origInfo.Instance);
      FreeAndNil(origInfo.Instance);
      FOriginals[AIndex] := origInfo;
    end;
  end;
end;

procedure TBGRALayeredBitmap.UnloadOriginal(const AGuid: TGuid);
begin
  UnloadOriginal(IndexOfOriginal(AGuid));
end;

destructor TBGRALayeredBitmap.Destroy;
begin
  FOriginalEditor.Free;
  inherited Destroy;
end;

constructor TBGRALayeredBitmap.Create;
begin
  inherited Create;
  FWidth := 0;
  FHeight := 0;
  FNbLayers:= 0;
  FOriginals := nil;
end;

constructor TBGRALayeredBitmap.Create(AWidth, AHeight: integer);
begin
  inherited Create;
  if AWidth < 0 then
    FWidth := 0
  else
    FWidth := AWidth;
  if AHeight < 0 then
    FHeight := 0
  else
    FHeight := AHeight;
  FNbLayers:= 0;
end;

function TBGRALayeredBitmap.GetLayerBitmapCopy(layer: integer): TBGRABitmap;
begin
  result := GetLayerBitmapDirectly(layer).Duplicate;
end;

function TBGRALayeredBitmap.GetLayerIndexFromId(AIdentifier: integer): integer;
var i: integer;
begin
  for i := 0 to NbLayers-1 do
    if FLayers[i].UniqueId = AIdentifier then
    begin
      result := i;
      exit;
    end;
  result := -1; //not found
end;

function TBGRALayeredBitmap.Duplicate(ASharedLayerIds: boolean): TBGRALayeredBitmap;
begin
  result := TBGRALayeredBitmap.Create;
  result.Assign(self, ASharedLayerIds);
end;

function TBGRALayeredBitmap.ProduceLayerUniqueId: integer;
begin
  result := InterLockedIncrement(NextLayerUniqueId);
end;

procedure TBGRALayeredBitmap.RotateCW;
var i: integer;
  newBmp: TBGRABitmap;
  newOfs: TPointF;
  m: TAffineMatrix;
begin
  SetSize(Height,Width); //unfreeze
  m := AffineMatrixTranslation(Width,0)*AffineMatrixRotationDeg(90);
  for i := 0 to NbLayers-1 do
  begin
    if Assigned(OnActionProgress) then OnActionProgress(self, round(i*100/NbLayers));
    newOfs:= m*PointF(FLayers[i].x,FLayers[i].y+FLayers[i].Source.Height);
    newBmp := FLayers[i].Source.RotateCW;
    if FLayers[i].Owner then FreeAndNil(FLayers[i].Source);
    FLayers[i].Source := newBmp;
    FLayers[i].Owner := true;
    FLayers[i].x := round(newOfs.x);
    FLayers[i].y := round(newOfs.y);
    FLayers[i].OriginalMatrix := m*FLayers[i].OriginalMatrix;
  end;
  if Assigned(OnActionDone) then OnActionDone(self);
end;

procedure TBGRALayeredBitmap.RotateCCW;
var i: integer;
  newBmp: TBGRABitmap;
  newOfs: TPointF;
  m: TAffineMatrix;
begin
  SetSize(Height,Width); //unfreeze
  m := AffineMatrixTranslation(0,Height)*AffineMatrixRotationDeg(-90);
  for i := 0 to NbLayers-1 do
  begin
    if Assigned(OnActionProgress) then OnActionProgress(self, round(i*100/NbLayers));
    newOfs:= m*PointF(FLayers[i].x+FLayers[i].Source.Width,FLayers[i].y);
    newBmp := FLayers[i].Source.RotateCCW;
    if FLayers[i].Owner then FreeAndNil(FLayers[i].Source);
    FLayers[i].Source := newBmp;
    FLayers[i].Owner := true;
    FLayers[i].x := round(newOfs.x);
    FLayers[i].y := round(newOfs.y);
    FLayers[i].OriginalMatrix := m*FLayers[i].OriginalMatrix;
  end;
  if Assigned(OnActionDone) then OnActionDone(self);
end;

procedure TBGRALayeredBitmap.RotateUD;
var i: integer;
begin
  Unfreeze;
  for i := 0 to NbLayers-1 do
  begin
    if Assigned(OnActionProgress) then OnActionProgress(self, round(i*100/NbLayers));
    RotateUD(i);
  end;
  if Assigned(OnActionDone) then OnActionDone(self);
end;

procedure TBGRALayeredBitmap.RotateUD(ALayerIndex: integer);
begin
  if (ALayerIndex < 0) or (ALayerIndex >= NbLayers) then
    raise ERangeError.Create('Index out of bounds');
  Unfreeze(ALayerIndex);
  if FLayers[ALayerIndex].Owner then
    FLayers[ALayerIndex].Source.RotateUDInplace
  else
  begin
    FLayers[ALayerIndex].Source := FLayers[ALayerIndex].Source.RotateUD;
    FLayers[ALayerIndex].Owner := true;
  end;
  FLayers[ALayerIndex].x := Width-FLayers[ALayerIndex].x-FLayers[ALayerIndex].Source.Width;
  FLayers[ALayerIndex].y := Height-FLayers[ALayerIndex].y-FLayers[ALayerIndex].Source.Height;
  FLayers[ALayerIndex].OriginalMatrix := AffineMatrixTranslation(+Width/2,+Height/2)*AffineMatrixScale(-1,-1)*AffineMatrixTranslation(-Width/2,-Height/2)*FLayers[ALayerIndex].OriginalMatrix;
end;

procedure TBGRALayeredBitmap.HorizontalFlip;
var i: integer;
begin
  Unfreeze;
  for i := 0 to NbLayers-1 do
  begin
    if Assigned(OnActionProgress) then OnActionProgress(self, round(i*100/NbLayers));
    HorizontalFlip(i);
  end;
  if Assigned(OnActionDone) then OnActionDone(self);
end;

procedure TBGRALayeredBitmap.HorizontalFlip(ALayerIndex: integer);
begin
  if (ALayerIndex < 0) or (ALayerIndex >= NbLayers) then
    raise ERangeError.Create('Index out of bounds');
  Unfreeze(ALayerIndex);
  if FLayers[ALayerIndex].Owner then
    FLayers[ALayerIndex].Source.HorizontalFlip
  else
  begin
    FLayers[ALayerIndex].Source := FLayers[ALayerIndex].Source.Duplicate(True);
    FLayers[ALayerIndex].Source.HorizontalFlip;
    FLayers[ALayerIndex].Owner := true;
  end;
  FLayers[ALayerIndex].x := Width-FLayers[ALayerIndex].x-FLayers[ALayerIndex].Source.Width;
  FLayers[ALayerIndex].OriginalMatrix := AffineMatrixTranslation(+Width/2,0)*AffineMatrixScale(-1,1)*AffineMatrixTranslation(-Width/2,0)*FLayers[ALayerIndex].OriginalMatrix;
end;

procedure TBGRALayeredBitmap.VerticalFlip;
var i: integer;
begin
  Unfreeze;
  for i := 0 to NbLayers-1 do
  begin
    if Assigned(OnActionProgress) then OnActionProgress(self, round(i*100/NbLayers));
    VerticalFlip(i);
  end;
  if Assigned(OnActionDone) then OnActionDone(self);
end;

procedure TBGRALayeredBitmap.VerticalFlip(ALayerIndex: integer);
begin
  if (ALayerIndex < 0) or (ALayerIndex >= NbLayers) then
    raise ERangeError.Create('Index out of bounds');
  Unfreeze(ALayerIndex);
  if FLayers[ALayerIndex].Owner then
    FLayers[ALayerIndex].Source.VerticalFlip
  else
  begin
    FLayers[ALayerIndex].Source := FLayers[ALayerIndex].Source.Duplicate(True);
    FLayers[ALayerIndex].Source.VerticalFlip;
    FLayers[ALayerIndex].Owner := true;
  end;
  FLayers[ALayerIndex].y := Height-FLayers[ALayerIndex].y-FLayers[ALayerIndex].Source.Height;
  FLayers[ALayerIndex].OriginalMatrix := AffineMatrixTranslation(0,+Height/2)*AffineMatrixScale(1,-1)*AffineMatrixTranslation(0,-Height/2)*FLayers[ALayerIndex].OriginalMatrix;
end;

procedure TBGRALayeredBitmap.Resample(AWidth, AHeight: integer;
  AResampleMode: TResampleMode; AFineResampleFilter: TResampleFilter);
var i, prevWidth, prevHeight: integer;
    resampled: TBGRABitmap;
    oldFilter : TResampleFilter;
    dummyRect: TRect;
begin
  if (AWidth < 0) or (AHeight < 0) then
    raise exception.Create('Invalid size');
  prevWidth := Width;
  if prevWidth < 1 then prevWidth := AWidth;
  prevHeight := Height;
  if prevHeight < 1 then prevHeight := AHeight;
  SetSize(AWidth, AHeight); //unfreeze
  dummyRect := EmptyRect;
  for i := 0 to NbLayers-1 do
  begin
    if Assigned(OnActionProgress) then OnActionProgress(self, round(i*100/NbLayers));
    if (FLayers[i].OriginalGuid <> GUID_NULL) and LayerOriginalKnown[i] then
    begin
      LayerOriginalMatrix[i] := AffineMatrixScale(AWidth/prevWidth,AHeight/prevHeight)*LayerOriginalMatrix[i];
      if AResampleMode = rmFineResample then RenderLayerFromOriginalIfNecessary(i, false, dummyRect);
    end else
    begin
      if LayerBitmap[i].NbPixels <> 0 then
      begin
        oldFilter := LayerBitmap[i].ResampleFilter;
        LayerBitmap[i].ResampleFilter := AFineResampleFilter;
        resampled := LayerBitmap[i].Resample(max(1,round(LayerBitmap[i].Width*AWidth/prevWidth)),
          max(1,round(LayerBitmap[i].Height*AHeight/prevHeight)), AResampleMode);
        LayerBitmap[i].ResampleFilter := oldFilter;
        SetLayerBitmap(i, resampled, True);
      end;
      with LayerOffset[i] do
        LayerOffset[i] := Point(round(X*AWidth/prevWidth),round(Y*AHeight/prevHeight));
    end;
  end;
  if Assigned(OnActionDone) then OnActionDone(self);
end;

procedure TBGRALayeredBitmap.SetLayerBitmap(layer: integer;
  ABitmap: TBGRABitmap; AOwned: boolean);
var
  layerDir: TMemDirectory;
begin
  if (layer < 0) or (layer >= NbLayers) then
    raise Exception.Create('Index out of bounds')
  else
  begin
    if ABitmap = FLayers[layer].Source then exit;
    Unfreeze(layer);
    if FLayers[layer].Owner then FLayers[layer].Source.Free;
    FLayers[layer].Source := ABitmap;
    FLayers[layer].Owner := AOwned;
    FLayers[layer].OriginalGuid := GUID_NULL;
    FLayers[layer].OriginalMatrix := AffineMatrixIdentity;
    layerDir := GetLayerDirectory(layer, false);
    if Assigned(layerDir) then
      layerDir.Delete(RenderSubDirectory,'');
  end;
end;

function TBGRALayeredBitmap.TakeLayerBitmap(layer: integer): TBGRABitmap;
begin
  result := GetLayerBitmapDirectly(layer);
  if Assigned(result) then
  begin
    if FLayers[layer].Owner then FLayers[layer].Owner := false
    else result := result.Duplicate;
  end;
end;

procedure TBGRALayeredBitmap.ApplyLayerOffset(ALayerIndex: integer;
  APadWithTranparentPixels: boolean);
var
  r: TRect;
  newBmp: TBGRABitmap;
begin
  if APadWithTranparentPixels then
  begin
    if (LayerOffset[ALayerIndex].X=0) and (LayerOffset[ALayerIndex].Y=0) and
       (LayerBitmap[ALayerIndex].Width=Width) and (LayerBitmap[ALayerIndex].Height=Height) then exit;
    newBmp := TBGRABitmap.Create(Width,Height);
    newBmp.PutImage(LayerOffset[ALayerIndex].X, LayerOffset[ALayerIndex].Y, LayerBitmap[ALayerIndex], dmSet);
    if FLayers[ALayerIndex].Owner then FLayers[ALayerIndex].Source.Free;
    FLayers[ALayerIndex].Source := newBmp;
    FLayers[ALayerIndex].Owner := true;
    FLayers[ALayerIndex].x := 0;
    FLayers[ALayerIndex].y := 0;
  end else
  begin
    if (LayerOffset[ALayerIndex].X>=0) and (LayerOffset[ALayerIndex].Y>=0) and
       (LayerOffset[ALayerIndex].X+LayerBitmap[ALayerIndex].Width <= Width) and
       (LayerOffset[ALayerIndex].Y+LayerBitmap[ALayerIndex].Height <= Height) then exit;
    r := RectWithSize(LayerOffset[ALayerIndex].X, LayerOffset[ALayerIndex].Y,
                      LayerBitmap[ALayerIndex].Width, LayerBitmap[ALayerIndex].Height);
    r.Intersect( rect(0,0,Width,Height) );
    newBmp := TBGRABitmap.Create(r.Width,r.Height);
    newBmp.PutImage(LayerOffset[ALayerIndex].X - r.Left, LayerOffset[ALayerIndex].Y - r.Top, LayerBitmap[ALayerIndex], dmSet);
    if FLayers[ALayerIndex].Owner then FLayers[ALayerIndex].Source.Free;
    FLayers[ALayerIndex].Source := newBmp;
    FLayers[ALayerIndex].Owner := true;
    FLayers[ALayerIndex].x := r.Left;
    FLayers[ALayerIndex].y := r.Top;
  end;
end;

function TBGRALayeredBitmap.DrawEditor(ADest: TBGRABitmap;
  ALayerIndex: integer; X, Y: Integer; APointSize: single): TRect;
begin
  result := DrawEditor(ADest, ALayerIndex, AffineMatrixTranslation(X,Y), APointSize);
end;

function TBGRALayeredBitmap.DrawEditor(ADest: TBGRABitmap; ALayerIndex: integer;
  AMatrix: TAffineMatrix; APointSize: single): TRect;
begin
  UpdateOriginalEditor(ALayerIndex, AMatrix, APointSize);
  if Assigned(OriginalEditor) then
    result := OriginalEditor.Render(ADest, rect(0,0,ADest.Width,ADest.Height))
    else result := EmptyRect;
end;

function TBGRALayeredBitmap.GetEditorBounds(ALayerIndex: integer; X,
  Y: Integer; APointSize: single): TRect;
begin
  result := GetEditorBounds(ALayerIndex, AffineMatrixTranslation(X,Y), APointSize);
end;

function TBGRALayeredBitmap.GetEditorBounds(ADestRect: TRect;
  ALayerIndex: integer; X, Y: Integer; APointSize: single): TRect;
begin
  result := GetEditorBounds(ADestRect, ALayerIndex, AffineMatrixTranslation(X,Y), APointSize);
end;

function TBGRALayeredBitmap.GetEditorBounds(ALayerIndex: integer;
  AMatrix: TAffineMatrix; APointSize: single): TRect;
begin
  result := GetEditorBounds(rect(-maxLongint,-maxLongint,maxLongint,maxLongint), ALayerIndex, AMatrix, APointSize);
end;

function TBGRALayeredBitmap.GetEditorBounds(ADestRect: TRect; ALayerIndex: integer;
  AMatrix: TAffineMatrix; APointSize: single): TRect;
begin
  UpdateOriginalEditor(ALayerIndex, AMatrix, APointSize);

  if Assigned(OriginalEditor) then
    result := OriginalEditor.GetRenderBounds(ADestRect)
    else result := EmptyRect;
end;

procedure TBGRALayeredBitmap.ClearEditor;
begin
  if Assigned(FOriginalEditor) then FOriginalEditor.Clear;
  FOriginalEditorOriginal := GUID_NULL;
end;

procedure TBGRALayeredBitmap.MouseMove(Shift: TShiftState; ImageX, ImageY: Single; out
  ACursor: TOriginalEditorCursor);
var
  handled: boolean;
begin
  MouseMove(Shift, ImageX,ImageY, ACursor, handled);
end;

procedure TBGRALayeredBitmap.MouseDown(RightButton: boolean;
  Shift: TShiftState; ImageX, ImageY: Single; out ACursor: TOriginalEditorCursor);
var
  handled: boolean;
begin
  MouseDown(RightButton, Shift, ImageX,ImageY, ACursor, handled);
end;

procedure TBGRALayeredBitmap.MouseUp(RightButton: boolean; Shift: TShiftState;
  ImageX, ImageY: Single; out ACursor: TOriginalEditorCursor);
var
  handled: boolean;
begin
  MouseUp(RightButton, Shift, ImageX,ImageY, ACursor, handled);
end;

procedure TBGRALayeredBitmap.MouseMove(Shift: TShiftState; ImageX, ImageY: Single; out
  ACursor: TOriginalEditorCursor; out AHandled: boolean);
var
  viewPt: TPointF;
begin
  if Assigned(OriginalEditor) then
  begin
    viewPt := FOriginalEditorViewMatrix*PointF(ImageX,ImageY);
    OriginalEditor.MouseMove(Shift, viewPt.X, viewPt.Y, ACursor, AHandled);
  end
  else
  begin
    ACursor:= oecDefault;
    AHandled:= false;
  end;
end;

procedure TBGRALayeredBitmap.MouseDown(RightButton: boolean;
  Shift: TShiftState; ImageX, ImageY: Single; out ACursor: TOriginalEditorCursor; out
  AHandled: boolean);
var
  viewPt: TPointF;
begin
  if Assigned(OriginalEditor) then
  begin
    viewPt := FOriginalEditorViewMatrix*PointF(ImageX,ImageY);
    OriginalEditor.MouseDown(RightButton, Shift, viewPt.X, viewPt.Y, ACursor, AHandled);
  end
  else
  begin
    ACursor:= oecDefault;
    AHandled:= false;
  end;
end;

procedure TBGRALayeredBitmap.MouseUp(RightButton: boolean; Shift: TShiftState;
  ImageX, ImageY: Single; out ACursor: TOriginalEditorCursor; out AHandled: boolean);
var
  viewPt: TPointF;
begin
  if Assigned(OriginalEditor) then
  begin
    viewPt := FOriginalEditorViewMatrix*PointF(ImageX,ImageY);
    OriginalEditor.MouseUp(RightButton, Shift, viewPt.X,viewPt.Y, ACursor, AHandled);
  end
  else
  begin
    ACursor:= oecDefault;
    AHandled:= false;
  end;
end;

procedure TBGRALayeredBitmap.KeyDown(Shift: TShiftState; Key: TSpecialKey; out
  AHandled: boolean);
begin
  if Assigned(OriginalEditor) then
    OriginalEditor.KeyDown(Shift, Key, AHandled)
  else
    AHandled := false;
end;

procedure TBGRALayeredBitmap.KeyUp(Shift: TShiftState; Key: TSpecialKey; out
  AHandled: boolean);
begin
  if Assigned(OriginalEditor) then
    OriginalEditor.KeyUp(Shift, Key, AHandled)
  else
    AHandled := false;
end;

procedure TBGRALayeredBitmap.KeyPress(UTF8Key: string; out AHandled: boolean);
begin
  if Assigned(OriginalEditor) then
    OriginalEditor.KeyPress(UTF8Key, AHandled)
  else
    AHandled := false;
end;

function TBGRALayeredBitmap.IndexOfOriginal(const AGuid: TGuid): integer;
var
  i: Integer;
begin
  for i := 0 to OriginalCount-1 do
    if FOriginals[i].Guid = AGuid then
    begin
      result := i;
      exit;
    end;
  result := -1
end;

function TBGRALayeredBitmap.IndexOfOriginal(AOriginal: TBGRALayerCustomOriginal): integer;
begin
  if Assigned(FOriginals) then
    result := FOriginals.IndexOf(BGRALayerOriginalEntry(AOriginal))
  else
    result := -1;
end;

{ TBGRACustomLayeredBitmap }

function TBGRACustomLayeredBitmap.GetLinearBlend: boolean;
begin
  result := FLinearBlend;
end;

function TBGRACustomLayeredBitmap.GetSelectionVisible: boolean;
begin
  result := (FSelectionScanner <> nil) and (FSelectionLayerIndex >= 0) and
    (FSelectionLayerIndex < NbLayers) and FSelectionRect.IntersectsWith(rect(0,0,Width,Height));
end;

function TBGRACustomLayeredBitmap.GetMemDirectory: TMemDirectory;
begin
  if FMemDirectory = nil then
  begin
    FMemDirectory:= TMemDirectory.Create;
    FMemDirectoryOwned := true;
  end;
  result := FMemDirectory;
end;

function TBGRACustomLayeredBitmap.GetDefaultBlendingOperation: TBlendOperation;
begin
  result := boTransparent;
end;

function TBGRACustomLayeredBitmap.GetHasMemFiles: boolean;
begin
  result := assigned(FMemDirectory) and (FMemDirectory.Count > 0);
end;

function TBGRACustomLayeredBitmap.GetLayerOriginalGuid(layer: integer): TGuid;
begin
  result := GUID_NULL;
end;

function TBGRACustomLayeredBitmap.GetLayerOriginalRenderStatus(layer: integer): TOriginalRenderStatus;
begin
  result := orsProof;
end;

function TBGRACustomLayeredBitmap.GetOriginalCount: integer;
begin
  result := 0;
end;

function TBGRACustomLayeredBitmap.GetOriginalByIndex(AIndex: integer): TBGRALayerCustomOriginal;
begin
  result := nil;
  raise exception.Create('Not implemented');
end;

function TBGRACustomLayeredBitmap.GetOriginalByIndexKnown(AIndex: integer): boolean;
begin
  result := true;
end;

function TBGRACustomLayeredBitmap.GetOriginalByIndexLoaded(AIndex: integer): boolean;
begin
  result := true;
end;

function TBGRACustomLayeredBitmap.GetOriginalByIndexClass(AIndex: integer): TBGRALayerOriginalAny;
begin
  result := nil;
end;

function TBGRACustomLayeredBitmap.GetLayerOriginal(layer: integer): TBGRALayerCustomOriginal;
begin
  result := nil;
end;

function TBGRACustomLayeredBitmap.GetLayerOriginalKnown(layer: integer): boolean;
begin
  result := true;
end;

function TBGRACustomLayeredBitmap.GetLayerOriginalMatrix(layer: integer): TAffineMatrix;
begin
  result := AffineMatrixIdentity;
end;

procedure TBGRACustomLayeredBitmap.SetLinearBlend(AValue: boolean);
begin
  Unfreeze;
  FLinearBlend := AValue;
end;

procedure TBGRACustomLayeredBitmap.SetMemDirectory(AValue: TMemDirectory);
begin
  if AValue = FMemDirectory then exit;
  if FMemDirectoryOwned then FMemDirectory.Free;
  FMemDirectory := AValue;
  FMemDirectoryOwned := false;
end;

function TBGRACustomLayeredBitmap.GetLayerName(layer: integer): string;
begin
  result := 'Layer' + inttostr(layer+1);
end;

{$hints off}
function TBGRACustomLayeredBitmap.GetLayerOffset(layer: integer): TPoint;
begin
  //optional function
  result := Point(0,0);
end;
{$hints on}

{$hints off}
function TBGRACustomLayeredBitmap.GetLayerBitmapDirectly(layer: integer
  ): TBGRABitmap;
begin
  //optional function
  result:= nil;
end;

function TBGRACustomLayeredBitmap.GetLayerFrozenRange(layer: integer): integer;
var i: integer;
begin
  for i := 0 to high(FFrozenRange) do
    if (layer >= FFrozenRange[i].firstLayer) and (layer <= FFrozenRange[i].lastLayer) then
    begin
      result := i;
      exit;
    end;
  result := -1;
end;

function TBGRACustomLayeredBitmap.GetLayerFrozen(layer: integer): boolean;
var i: integer;
begin
  for i := 0 to high(FFrozenRange) do
    if (layer >= FFrozenRange[i].firstLayer) and (layer <= FFrozenRange[i].lastLayer) then
    begin
      result := true;
      exit;
    end;
  result := false;
end;

function TBGRACustomLayeredBitmap.GetLayerUniqueId(layer: integer): integer;
begin
  result := layer;
end;

procedure TBGRACustomLayeredBitmap.SetLayerFrozen(layer: integer;
  AValue: boolean);
begin
  //nothing
end;

function TBGRACustomLayeredBitmap.RangeIntersect(first1, last1, first2,
  last2: integer): boolean;
begin
  result := (first1 <= last2) and (last1 >= first2);
end;

procedure TBGRACustomLayeredBitmap.RemoveFrozenRange(index: integer);
var j,i: integer;
begin
  for j := FFrozenRange[index].firstLayer to FFrozenRange[index].lastLayer do
    SetLayerFrozen(j,False);
  FFrozenRange[index].image.Free;
  for i := index to high(FFrozenRange)-1 do
    FFrozenRange[i] := FFrozenRange[i+1];
  setlength(FFrozenRange,length(FFrozenRange)-1);
end;

function TBGRACustomLayeredBitmap.ContainsFrozenRange(first, last: integer): boolean;
var i: integer;
begin
  for i := 0 to high(FFrozenRange) do
    if (FFrozenRange[i].firstLayer = first) and (FFrozenRange[i].lastLayer = last) then
    begin
      result := true;
      exit;
    end;
  result := false;
end;

function TBGRACustomLayeredBitmap.GetLayerDrawMode(AIndex: integer): TDrawMode;
begin
  if (BlendOperation[AIndex] = boTransparent) and not LinearBlend then
    result := dmDrawWithTransparency
    else result := dmLinearBlend;
end;

function TBGRACustomLayeredBitmap.GetEmpty: boolean;
begin
  result := (NbLayers = 0) and (Width = 0) and (Height = 0);
end;

function TBGRACustomLayeredBitmap.IndexOfOriginal(const AGuid: TGuid): integer;
begin
  result := -1;
end;

function TBGRACustomLayeredBitmap.IndexOfOriginal(
  AOriginal: TBGRALayerCustomOriginal): integer;
begin
  result := -1;
end;

procedure TBGRACustomLayeredBitmap.SetWidth(Value: Integer);
begin
  //nothing
end;

procedure TBGRACustomLayeredBitmap.SetHeight(Value: Integer);
begin
  //nothing
end;

function TBGRACustomLayeredBitmap.GetTransparent: Boolean;
begin
  result := true;
end;

procedure TBGRACustomLayeredBitmap.SetTransparent(Value: Boolean);
begin
  //nothing
end;

procedure TBGRACustomLayeredBitmap.SaveToFile(const filenameUTF8: string);
var bmp: TBGRABitmap;
    ext: string;
    temp: TBGRALayeredBitmap;
    i: integer;
    stream: TFileStreamUTF8;
begin
  ext := UTF8LowerCase(ExtractFileExt(filenameUTF8));
  for i := 0 to high(LayeredBitmapWriters) do
    if '.'+LayeredBitmapWriters[i].extension = ext then
    begin
      temp := LayeredBitmapWriters[i].theClass.Create;
      try
        temp.Assign(self);
        temp.SaveToFile(filenameUTF8);
      finally
        temp.Free;
      end;
      exit;
    end;

  //when using "data" extension, simply serialize
  if (ext='.dat') or (ext='.data') then
  begin
    if Assigned(LayeredBitmapLoadFromStreamProc) then
    begin
      stream := TFileStreamUTF8.Create(filenameUTF8, fmCreate);
      try
        LayeredBitmapSaveToStreamProc(stream, self);
      finally
        stream.Free;
      end;
    end else
      raise exception.Create('Enable layer serialization by calling BGRAStreamLayers.RegisterStreamLayers');
  end else
  begin
    bmp := ComputeFlatImage;
    try
      bmp.SaveToFileUTF8(filenameUTF8);
    finally
      bmp.Free;
    end;
  end;
end;

procedure TBGRACustomLayeredBitmap.SaveToStream(Stream: TStream);
begin
  if Assigned(LayeredBitmapSaveToStreamProc) then
    LayeredBitmapSaveToStreamProc(Stream, self)
  else
    raise exception.Create('Call BGRAStreamLayers.RegisterStreamLayers first');
end;

procedure TBGRACustomLayeredBitmap.SaveToStreamAs(Stream: TStream;
  AExtension: string);
var bmp: TBGRABitmap;
    ext: string;
    format: TBGRAImageFormat;
    temp: TBGRALayeredBitmap;
    i: integer;
begin
  ext := UTF8LowerCase(AExtension);
  if ext[1] <> '.' then ext := '.'+ext;

  for i := 0 to high(LayeredBitmapWriters) do
    if '.'+LayeredBitmapWriters[i].extension = ext then
    begin
      temp := LayeredBitmapWriters[i].theClass.Create;
      try
        temp.Assign(self, true, true);
        temp.SaveToStream(Stream);
      finally
        temp.Free;
      end;
      exit;
    end;

  format := SuggestImageFormat(ext);
  bmp := ComputeFlatImage;
  try
    bmp.SaveToStreamAs(Stream, format);
  finally
    bmp.Free;
  end;
end;

constructor TBGRACustomLayeredBitmap.Create;
begin
  FFrozenRange := nil;
  FLinearBlend:= True;
  FMemDirectory := nil;
  FMemDirectoryOwned:= false;
  FSelectionDrawMode:= dmDrawWithTransparency;
  FSelectionLayerIndex:= -1;
  FSelectionRect:= EmptyRect;
  FSelectionScanner:= nil;
  FSelectionScannerOffset:= Point(0,0);
end;

{$hints on}

function TBGRACustomLayeredBitmap.ToString: ansistring;
var
  i: integer;
begin
  Result := 'LayeredBitmap' + LineEnding + LineEnding;
  for i := 0 to NbLayers - 1 do
  begin
    AppendStr(Result, LineEnding + 'Layer ' + IntToStr(i) + ' : ' + LayerName[i] + LineEnding);
  end;
end;

procedure TBGRACustomLayeredBitmap.DiscardSelection;
begin
  fillchar(FSelectionScanner, sizeof(FSelectionScanner), 0);
  FSelectionRect := EmptyRect;
  FSelectionLayerIndex := -1;
  FSelectionScannerOffset:= Point(0,0);
end;

function TBGRACustomLayeredBitmap.ComputeFlatImage(ASeparateXorMask: boolean): TBGRABitmap;
begin
  result := ComputeFlatImage(rect(0,0,Width,Height), 0, NbLayers - 1, ASeparateXorMask);
end;

function TBGRACustomLayeredBitmap.ComputeFlatImage(firstLayer,
  lastLayer: integer; ASeparateXorMask: boolean): TBGRABitmap;
begin
  result := ComputeFlatImage(rect(0,0,Width,Height), firstLayer,LastLayer,ASeparateXorMask);
end;

function TBGRACustomLayeredBitmap.ComputeFlatImage(ARect: TRect;
  ASeparateXorMask: boolean): TBGRABitmap;
begin
  result := ComputeFlatImage(ARect,0, NbLayers - 1, ASeparateXorMask);
end;

destructor TBGRACustomLayeredBitmap.Destroy;
begin
  DiscardSelection;
  Clear;
end;

function TBGRACustomLayeredBitmap.ComputeFlatImage(ARect: TRect; firstLayer, lastLayer: integer; ASeparateXorMask: boolean): TBGRABitmap;
var
  i,j: integer;
  destEmpty: boolean;

begin
  if (firstLayer < 0) or (lastLayer > NbLayers-1) then
    raise ERangeError.Create('Layer index out of bounds');
  If (ARect.Right <= ARect.Left) or (ARect.Bottom <= ARect.Top) then
  begin
    result := TBGRABitmap.Create(0,0);
    exit;
  end;
  Result := TBGRABitmap.Create(ARect.Right-ARect.Left, ARect.Bottom-ARect.Top);
  destEmpty := true;
  if SelectionVisible then Unfreeze(SelectionLayerIndex);
  i := firstLayer;
  while i <= lastLayer do
  begin
    if LayerFrozen[i] then
    begin
      j := GetLayerFrozenRange(i);
      if j <> -1 then
      begin
        if i = 0 then
          Result.PutImage(-ARect.Left,-ARect.Top,FFrozenRange[j].image,dmSet) else
        if not FFrozenRange[j].linearBlend then
          Result.PutImage(-ARect.Left,-ARect.Top,FFrozenRange[j].image,dmDrawWithTransparency)
        else
          Result.PutImage(-ARect.Left,-ARect.Top,FFrozenRange[j].image,dmLinearBlend);
        i := FFrozenRange[j].lastLayer+1;
        destEmpty := false;
        continue;
      end;
    end;
    if DrawLayer(result, -ARect.Left, -ARect.Top, i, ASeparateXorMask, destEmpty) then
      destEmpty := false;
    inc(i);
  end;
  if result.XorMask <> nil then
    AlphaFillInline(result.XorMask.Data, 0, result.XorMask.NbPixels);
end;

procedure TBGRACustomLayeredBitmap.Draw(ACanvas: TCanvas; const Rect: TRect);
var temp: TBGRABitmap;
begin
  if (Rect.Right <= Rect.Left) or (Rect.Bottom <= Rect.Top) then exit;
  if (Rect.Right-Rect.Left = Width) and (Rect.Bottom-Rect.Top = Height) then
    Draw(ACanvas, Rect.Left,Rect.Top) else
  begin
    temp := ComputeFlatImage;
    BGRAReplace(temp,temp.Resample(Rect.Right-Rect.Left,Rect.Bottom-Rect.Top));
    temp.Draw(ACanvas, Rect.Left,Rect.Top, False);
    temp.Free;
  end;
end;

procedure TBGRACustomLayeredBitmap.Draw(Canvas: TCanvas; x, y: integer);
begin
  Draw(Canvas,x,y,0,NbLayers-1);
end;

procedure TBGRACustomLayeredBitmap.Draw(Canvas: TCanvas; x, y: integer; firstLayer, lastLayer: integer);
var temp: TBGRABitmap;
begin
  temp := ComputeFlatImage(firstLayer,lastLayer);
  temp.Draw(Canvas,x,y,False);
  temp.Free;
end;

procedure TBGRACustomLayeredBitmap.Draw(Dest: TBGRABitmap; x, y: integer);
begin
  Draw(Dest, x, y, 0, NbLayers-1);
end;

procedure TBGRACustomLayeredBitmap.Draw(Dest: TBGRABitmap; x, y: integer;
  ASeparateXorMask: boolean; ADestinationEmpty: boolean);
begin
  Draw(Dest, x, y, 0, NbLayers-1, ASeparateXorMask, ADestinationEmpty);
end;

procedure TBGRACustomLayeredBitmap.Draw(Dest: TBGRABitmap; AX, AY: integer; firstLayer, lastLayer: integer; ASeparateXorMask: boolean; ADestinationEmpty: boolean);
var
  temp: TBGRABitmap;
  i,j: integer;
  NewClipRect: TRect;
begin
  NewClipRect := TRect.Intersect(rect(AX,AY,AX+Width,AY+Height), Dest.ClipRect);
  if NewClipRect.IsEmpty then exit;

  for i := firstLayer to lastLayer do
    if LayerVisible[i] and
      (not (BlendOperation[i] in[boTransparent,boLinearBlend]) or
       ( (SelectionLayerIndex = i) and SelectionVisible
         and (SelectionDrawMode <> GetLayerDrawMode(i)) ) ) then
    begin
      temp := ComputeFlatImage(rect(NewClipRect.Left-AX,NewClipRect.Top-AY,NewClipRect.Right-AX,NewClipRect.Bottom-AY), ASeparateXorMask);
      if ADestinationEmpty then
        Dest.PutImage(NewClipRect.Left, NewClipRect.Top, temp, dmSet) else
      if self.LinearBlend then
        Dest.PutImage(NewClipRect.Left, NewClipRect.Top, temp, dmLinearBlend)
        else Dest.PutImage(NewClipRect.Left, NewClipRect.Top, temp, dmDrawWithTransparency);
      temp.Free;
      exit;
    end;

  i := firstLayer;
  while i <= lastLayer do
  begin
    if LayerFrozen[i] then
    begin
      j := GetLayerFrozenRange(i);
      if j <> -1 then
      begin
        if ADestinationEmpty then
          Dest.PutImage(AX, AY, FFrozenRange[j].image, dmSet) else
        if not FFrozenRange[j].linearBlend then
          Dest.PutImage(AX, AY, FFrozenRange[j].image, dmDrawWithTransparency)
          else Dest.PutImage(AX, AY, FFrozenRange[j].image, dmLinearBlend);
        i := FFrozenRange[j].lastLayer+1;
        ADestinationEmpty := false;
        continue;
      end;
    end;
    if DrawLayer(Dest, AX,AY, i, ASeparateXorMask, ADestinationEmpty) then
      ADestinationEmpty := false;
    inc(i);
  end;
end;

function TBGRACustomLayeredBitmap.DrawLayer(Dest: TBGRABitmap; X, Y: Integer;
  AIndex: integer; ASeparateXorMask: boolean; ADestinationEmpty: boolean): boolean;
type IntArray4 = array[1..4] of integer;

  function MergeSort(const ATab: IntArray4): IntArray4;
  var
    posA, posB, pos: Integer;
  begin
    posA := 1;
    posB := 3;
    pos := 1;
    while (posA <= 2) and (posB <= 4) do
    begin
      if ATab[posA] <= ATab[posB] then
      begin
        result[pos] := ATab[posA];
        inc(posA);
      end else
      begin
        result[pos] := ATab[posB];
        inc(posB);
      end;
      inc(pos);
    end;
    while posA <= 2 do
    begin
      result[pos] := ATab[posA];
      inc(posA); inc(pos);
    end;
    while posB <= 4 do
    begin
      result[pos] := ATab[posB];
      inc(posB); inc(pos);
    end;
  end;

var
  opacity: Byte;

  procedure Blend(ADestRect: TRect; AScan: IBGRAScanner; AScanOfsX, AScanOfsY: integer; ABlendOp: TBlendOperation);
  begin
    //XOR mask
    if (ABlendOp = boXor) and ASeparateXorMask then
    begin
      Dest.NeedXorMask;
      Dest.XorMask.BlendImageOver(ADestRect, AScan, AScanOfsX, AScanOfsY, ABlendOp, opacity, LinearBlend);
    end else
    //first layer is simply the background
    if ADestinationEmpty and (ABlendOp <> boMask) then
    begin
      Dest.FillRect(ADestRect, AScan, dmSet, Point(AScanOfsX, AScanOfsY));
      Dest.ApplyGlobalOpacity(ADestRect, opacity);
    end
    else
      Dest.BlendImageOver(ADestRect, AScan, AScanOfsX, AScanOfsY, ABlendOp, opacity, LinearBlend);
  end;

var
  tempLayer: TBGRABitmap;
  tempLayerScanOfs, selScanOfs: TPoint;
  blendOp: TBlendOperation;

  procedure BlendBoth(ATile: TRect);
  var
    mergeBuf: PByte;
    pTemp: PByte;
    tempStride, rowSize, destStride: PtrInt;
    tileWidth, yb: LongInt;
    pDest: PByte;
  begin
    tileWidth := ATile.Width;
    rowSize := tileWidth * sizeof(TBGRAPixel);
    if not ADestinationEmpty then
      getmem(mergeBuf, rowSize)
      else mergeBuf := nil;
    try
      if tempLayer.LineOrder = riloTopToBottom then
        tempStride := tempLayer.RowSize else tempStride := -tempLayer.RowSize;
      pTemp := tempLayer.GetPixelAddress(ATile.Left + tempLayerScanOfs.X,
                 ATile.Top + tempLayerScanOfs.Y);
      pDest := Dest.GetPixelAddress(ATile.Left, ATile.Top);
      if Dest.LineOrder = riloTopToBottom then
        destStride := Dest.RowSize else destStride := -Dest.RowSize;
      if ADestinationEmpty then
      begin
        for yb := ATile.Top to ATile.Bottom-1 do
        begin
          move(pTemp^, pDest^, rowSize);
          SelectionScanner.ScanMoveTo(ATile.Left + selScanOfs.X, yb + selScanOfs.Y);
          ScannerPutPixels(SelectionScanner, PBGRAPixel(pDest), tileWidth, SelectionDrawMode);
          inc(pTemp, tempStride);
          inc(pDest, destStride);
        end;
        Dest.ApplyGlobalOpacity(ATile, opacity);
      end else
      begin
        for yb := ATile.Top to ATile.Bottom-1 do
        begin
          move(pTemp^, mergeBuf^, rowSize);
          SelectionScanner.ScanMoveTo(ATile.Left + selScanOfs.X, yb + selScanOfs.Y);
          ScannerPutPixels(SelectionScanner, PBGRAPixel(mergeBuf), tileWidth, SelectionDrawMode);
          BlendPixelsOver(PBGRAPixel(pDest), PBGRAPixel(mergeBuf),
              blendOp, tileWidth, opacity, LinearBlend);
          inc(pTemp, tempStride);
          inc(pDest, destStride);
        end;
      end;
    finally
      freemem(mergeBuf);
    end;
  end;

var
  mustFreeCopy, containsSel, containsLayer: Boolean;
  ofs: TPoint;
  rSel, oldClip, rLayer, rTile: TRect;
  xTab,yTab: IntArray4;
  xb, yb: Integer;
begin
  if not LayerVisible[AIndex] then exit(false);
  opacity := LayerOpacity[AIndex];
  if opacity = 0 then exit(false);

  tempLayer := GetLayerBitmapDirectly(AIndex);
  if tempLayer <> nil then mustFreeCopy := false else
    begin
      mustFreeCopy := true;
      tempLayer := GetLayerBitmapCopy(AIndex);
    end;

  ofs := LayerOffset[AIndex];
  oldClip := Dest.IntersectClip(rect(X,Y,X+self.Width,Y+self.Height));

  if (SelectionLayerIndex = AIndex) and SelectionVisible then
  begin
    rSel := SelectionRect;
    rSel.Offset(X, Y);
    rSel.Intersect(Dest.ClipRect);
  end else
    rSel := EmptyRect;

  if Assigned(tempLayer) then
  begin
    rLayer := RectWithSize(ofs.x + X, ofs.y + Y, tempLayer.Width, tempLayer.Height);
    rLayer.Intersect(Dest.ClipRect);
  end else
    rLayer := EmptyRect;

  if (tempLayer <> nil) and (not rLayer.IsEmpty or not rSel.IsEmpty) then
  begin
    if AIndex = 0 then blendOp := boTransparent else blendOp := BlendOperation[AIndex];
    tempLayerScanOfs := Point(-(ofs.X+X), -(ofs.Y+Y));

    if rSel.IsEmpty then
      Blend(rLayer, tempLayer, tempLayerScanOfs.X, tempLayerScanOfs.y, blendOp)
    else
    begin
      selScanOfs := Point(SelectionScannerOffset.X - X, SelectionScannerOffset.Y - Y);

      xTab[1] := rSel.Left;    yTab[1] := rSel.Top;
      xTab[2] := rSel.Right;   yTab[2] := rSel.Bottom;
      xTab[3] := rLayer.Left;  yTab[3] := rLayer.Top;
      xTab[4] := rLayer.Right; yTab[4] := rLayer.Bottom;
      xTab := MergeSort(xTab); yTab := MergeSort(yTab);

      for yb := 1 to 3 do
      begin
        rTile.Top := yTab[yb];
        rTile.Bottom := yTab[yb+1];
        if rTile.Bottom > rTile.Top then
          for xb := 1 to 3 do
          begin
            rTile.Left := xTab[xb];
            rTile.Right := xTab[xb+1];
            if rTile.Right > rTile.Left then
            begin
              containsSel := rTile.IntersectsWith(rSel);
              containsLayer := rTile.IntersectsWith(rLayer);
              if containsLayer then
              begin
                if not containsSel then
                  Blend(rTile, tempLayer, tempLayerScanOfs.X, tempLayerScanOfs.y, blendOp)
                else
                  BlendBoth(rTile);
              end else
              if containsSel then
                Blend(rTile, SelectionScanner, selScanOfs.X, selScanOfs.Y, blendOp)
            end;
          end;
      end;
    end;

    result := true;
  end else
    result := false;

  Dest.ClipRect := oldClip;
  if mustFreeCopy then tempLayer.Free;
end;

procedure TBGRACustomLayeredBitmap.FreezeExceptOneLayer(layer: integer);
begin
  if (layer < 0) or (layer >= NbLayers) then
  begin
    Freeze;
    exit;
  end;
  Unfreeze(layer,layer);
  if layer > 1 then
    Freeze(0,layer-1);
  if layer < NbLayers-2 then
    Freeze(layer+1,NbLayers-1);
end;

procedure TBGRACustomLayeredBitmap.Freeze(firstLayer, lastLayer: integer);

  procedure DoFreeze(first,last: integer; linear: boolean);
  var i,nbVisible: integer;
    computedImage: TBGRABitmap;
  begin
    if last <= first then exit; //at least 2 frozen layers
    nbVisible := 0;
    for i := first to last do
      if LayerVisible[i] and (LayerOpacity[i] > 0) then inc(nbVisible);
    if nbvisible < 2 then exit;  //at least 2 frozen layers

    if ContainsFrozenRange(first,last) then exit; //already frozen
    Unfreeze(first,last);

    computedImage := ComputeFlatImage(first,last); //must compute before layers are considered as frozen
    setlength(FFrozenRange, length(FFrozenRange)+1);
    with FFrozenRange[high(FFrozenRange)] do
    begin
      firstLayer := first;
      lastLayer:= last;
      image := computedImage;
      linearBlend := linear;
    end;
    for i := first to last do
      SetLayerFrozen(i,True);
  end;

var j: integer;
  start: integer;
  linear,nextLinear: boolean;
begin
  start := -1;
  linear := false; //to avoid hint
  for j := firstlayer to lastLayer do
  if ((BlendOperation[j] in [boTransparent,boLinearBlend]) or (start = 0) or ((firstlayer= 0) and (j=0)))
     and (not SelectionVisible or (j <> SelectionLayerIndex)) then
  begin
    nextLinear := (BlendOperation[j] = boLinearBlend) or self.LinearBlend;
    if start = -1 then
    begin
      start := j;
      linear := nextLinear;
    end else
    begin
      if linear <> nextLinear then
      begin
        DoFreeze(start,j-1,linear);
        start := j;
        linear := nextLinear;
      end;
    end;
  end else
  begin
    if start <> -1 then
    begin
      DoFreeze(start,j-1,linear);
      start := -1;
    end;
  end;
  if start <> -1 then
    DoFreeze(start,lastLayer,linear);
end;

procedure TBGRACustomLayeredBitmap.Freeze;
begin
  Freeze(0,NbLayers-1);
end;

procedure TBGRACustomLayeredBitmap.Unfreeze;
begin
  Unfreeze(0,NbLayers-1);
end;

procedure TBGRACustomLayeredBitmap.Unfreeze(layer: integer);
begin
  Unfreeze(layer,layer);
end;

procedure TBGRACustomLayeredBitmap.Unfreeze(firstLayer, lastLayer: integer);
var i: integer;
begin
  for i := high(FFrozenRange) downto 0 do
    if RangeIntersect(firstLayer,lastLayer,FFrozenRange[i].firstLayer,FFrozenRange[i].lastLayer) then
      RemoveFrozenRange(i);
end;

procedure TBGRACustomLayeredBitmap.NotifyLoaded;
begin
  //nothing
end;

procedure TBGRACustomLayeredBitmap.NotifySaving;
begin
  //nothing
end;

procedure RegisterLayeredBitmapReader(AExtensionUTF8: string; AReader: TBGRACustomLayeredBitmapClass);
begin
  setlength(LayeredBitmapReaders,length(LayeredBitmapReaders)+1);
  with LayeredBitmapReaders[high(LayeredBitmapReaders)] do
  begin
    extension:= UTF8LowerCase(AExtensionUTF8);
    theClass := AReader;
  end;
end;

function TryCreateLayeredBitmapWriter(AExtensionUTF8: string): TBGRALayeredBitmap;
var
  i: Integer;
begin
  AExtensionUTF8:= UTF8LowerCase(AExtensionUTF8);
  if (AExtensionUTF8 = '') or (AExtensionUTF8[1] <> '.') then
    AExtensionUTF8:= '.'+AExtensionUTF8;
  for i := 0 to high(LayeredBitmapWriters) do
    if '.'+LayeredBitmapWriters[i].extension = AExtensionUTF8 then
    begin
      result := LayeredBitmapWriters[i].theClass.Create;
      exit;
    end;
  result := nil;
end;

function TryCreateLayeredBitmapReader(AExtensionUTF8: string): TBGRACustomLayeredBitmap;
var
  i: Integer;
begin
  AExtensionUTF8:= UTF8LowerCase(AExtensionUTF8);
  if (AExtensionUTF8 = '') or (AExtensionUTF8[1] <> '.') then
    AExtensionUTF8:= '.'+AExtensionUTF8;
  for i := 0 to high(LayeredBitmapReaders) do
    if '.'+LayeredBitmapReaders[i].extension = AExtensionUTF8 then
    begin
      result := LayeredBitmapReaders[i].theClass.Create;
      exit;
    end;
  result := nil;
end;

procedure OnLayeredBitmapLoadFromStreamStart;
begin
  OnLayeredBitmapLoadStart('<Stream>');
end;

procedure OnLayeredBitmapLoadStart(AFilenameUTF8: string);
var i: Integer;
begin
  with LayeredBitmapLoadEvents do if Assigned(OnStart) then
    for i := 0 to OnStart.Count-1 do OnStart[i](AFilenameUTF8);
end;

procedure OnLayeredBitmapLoadProgress(APercentage: integer);
var i: Integer;
begin
  with LayeredBitmapLoadEvents do if Assigned(OnProgress) then
    for i := 0 to OnProgress.Count-1 do OnProgress[i](APercentage);
end;

procedure OnLayeredBitmapLoaded;
var i: Integer;
begin
  with LayeredBitmapLoadEvents do if Assigned(OnDone) then
    for i := 0 to OnDone.Count-1 do OnDone[i];
end;

procedure RegisterLoadingHandler(AStart: TOnLayeredBitmapLoadStartProc;
  AProgress: TOnLayeredBitmapLoadProgressProc; ADone: TOnLayeredBitmapLoadedProc);
begin
  with LayeredBitmapLoadEvents do begin
    if (AStart <> nil) and ((OnStart = nil) or (OnStart.IndexOf(AStart) = -1)) then
    begin
      if OnStart = nil then OnStart := TOnLayeredBitmapLoadStartProcList.Create;
      OnStart.Add(AStart);
    end;
    if (AProgress <> nil) and ((OnProgress = nil) or (OnProgress.IndexOf(AProgress) = -1)) then
    begin
      if OnProgress = nil then OnProgress := TOnLayeredBitmapLoadProgressProcList.Create;
      OnProgress.Add(AProgress);
    end;
    if (ADone <> nil) and ((OnDone = nil) or (OnDone.IndexOf(ADone) = -1)) then
    begin
      if OnDone = nil then OnDone := TOnLayeredBitmapLoadedProcList.Create;
      OnDone.Add(ADone);
    end;
  end;
end;

procedure UnregisterLoadingHandler(AStart: TOnLayeredBitmapLoadStartProc;
  AProgress: TOnLayeredBitmapLoadProgressProc; ADone: TOnLayeredBitmapLoadedProc);
begin
  with LayeredBitmapLoadEvents do begin
    if Assigned(OnStart) then OnStart.Remove(AStart);
    if Assigned(OnProgress) then OnProgress.Remove(AProgress);
    if Assigned(OnDone) then OnDone.Remove(ADone);
  end;
end;

procedure OnLayeredBitmapSaveToStreamStart;
begin
  OnLayeredBitmapSaveStart('<Stream>');
end;

procedure OnLayeredBitmapSaveStart(AFilenameUTF8: string);
var i: Integer;
begin
  with LayeredBitmapSaveEvents do if Assigned(OnStart) then
    for i := 0 to OnStart.Count-1 do OnStart[i](AFilenameUTF8);
end;

procedure OnLayeredBitmapSaveProgress(APercentage: integer);
var i: Integer;
begin
  with LayeredBitmapSaveEvents do if Assigned(OnProgress) then
    for i := 0 to OnProgress.Count-1 do OnProgress[i](APercentage);
end;

procedure OnLayeredBitmapSaved;
var i: Integer;
begin
  with LayeredBitmapSaveEvents do if Assigned(OnDone) then
    for i := 0 to OnDone.Count-1 do OnDone[i];
end;

procedure RegisterSavingHandler(AStart: TOnLayeredBitmapSaveStartProc;
  AProgress: TOnLayeredBitmapSaveProgressProc; ADone: TOnLayeredBitmapSavedProc);
begin
  with LayeredBitmapSaveEvents do begin
    if (AStart <> nil) and ((OnStart = nil) or (OnStart.IndexOf(AStart) = -1)) then
    begin
      if OnStart = nil then OnStart := TOnLayeredBitmapSaveStartProcList.Create;
      OnStart.Add(AStart);
    end;
    if (AProgress <> nil) and ((OnProgress = nil) or (OnProgress.IndexOf(AProgress) = -1)) then
    begin
      if OnProgress = nil then OnProgress := TOnLayeredBitmapSaveProgressProcList.Create;
      OnProgress.Add(AProgress);
    end;
    if (ADone <> nil) and ((OnDone = nil) or (OnDone.IndexOf(ADone) = -1)) then
    begin
      if OnDone = nil then OnDone := TOnLayeredBitmapSavedProcList.Create;
      OnDone.Add(ADone);
    end;
  end;
end;

procedure UnregisterSavingHandler(AStart: TOnLayeredBitmapSaveStartProc;
  AProgress: TOnLayeredBitmapSaveProgressProc; ADone: TOnLayeredBitmapSavedProc);
begin
  with LayeredBitmapSaveEvents do begin
    if Assigned(OnStart) then OnStart.Remove(AStart);
    if Assigned(OnProgress) then OnProgress.Remove(AProgress);
    if Assigned(OnDone) then OnDone.Remove(ADone);
  end;
end;

procedure RegisterLayeredBitmapWriter(AExtensionUTF8: string; AWriter: TBGRALayeredBitmapClass);
begin
  while (length(AExtensionUTF8)>0) and (AExtensionUTF8[1]='.') do delete(AExtensionUTF8,1,1);
  setlength(LayeredBitmapWriters,length(LayeredBitmapWriters)+1);
  with LayeredBitmapWriters[high(LayeredBitmapWriters)] do
  begin
    extension:= UTF8LowerCase(AExtensionUTF8);
    theClass := AWriter;
  end;
end;

initialization

  NextLayerUniqueId := 1;

finalization

  with LayeredBitmapLoadEvents do begin
    OnStart.Free;
    OnProgress.Free;
    OnDone.Free;
  end;
  with LayeredBitmapSaveEvents do begin
    OnStart.Free;
    OnProgress.Free;
    OnDone.Free;
  end;

end.

