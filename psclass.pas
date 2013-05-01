{ Don Whitbeck 2010 - Basic class for postscript output
}
unit psclass;
//{$linklib c}
{$mode objfpc} 
interface
 uses BaseUnix, unixtype,initc, errors, sysutils, classes, strings, FileUtil, Dialogs,
   Graphics, GraphType, OSPRinters, Printers,  Process, ctypes, db, sqldb;

const
  CR = #13;
  LF = #10;
  LETTERWIDTH  = 612;
  LETTERHEIGHT = 792;
  JUSTIFYLEFT = 0;
  JUSTIFYCENTER = 1;
  JUSTIFYRIGHT = 2;
  TIMESROMAN = 'Times-Roman';
  TIMESITALIC = 'Times-Italic';
  TIMESBOLD = 'Times-Bold';
  HELVETICA = 'Helvetica';
  HELVETICAITALIC = 'Helvetica-Italic';
  HELVETICABOLD = 'Helvetica-Bold';
  HELVETICACONDENSED = 'Helvetica-Condensed';
  POINTS = 72.0;

  BOXSPACE  = 6;      //Extra height to fit current font
  BOXMARGIN = 4;      //Margin for left or right between box sides and text

  BOXLINEALL = 15;
  BOXLINENONE = 0;
  BOXLINELEFT = 1;
  BOXLINETOP = 2;
  BOXLINERIGHT = 4;
  BOXLINEBOTTOM = 8;

  RELATIVE = TRUE;
  ABSOLUT = FALSE;


Type
   PrinterMargins = record
      TopMargin:    Integer;
      LeftMargin:   Integer;
      RightMargin:  Integer;
      BottomMargin: Integer;
   end;

Type
   PrinterDotsPI = record
     XDotsPI: Integer;
     YDotsPI: Integer;
   end;

{     TRect = record
    case Integer of
      0: (Left, Top, Right, Bottom: Integer);
      1: (TopLeft, BottomRight: TPoint);
  end;    }

Type
  PFontType = ^FontType;
  FontType = record
    FontName: String;
    FontSize: Integer;
    FontHeight: Integer;
  end;

Type
  TFontArray = array [1..10] of PFontType;

Type
  PTab = ^TTab;                  //A pointer to a tab
	TTab = record
          XPos:         Integer;       //tab stop - use integers to reduce conversions
	  justifyText:  Integer;
	  BoxWidth:     Integer;       //width of this tab box
	  Margin:       Integer;       //Distance from left tab edge and start of text
	  BShade:       TGraphicsColor;          //Shadeing in box image
          PSBShade:     byte;                    //shading in box postscript
	  BLines:       byte;          //Box lines
	  Next:         PTab;
	  Prev:         PTab;
	end;

Type
    PTabList = ^TTabList;            //A pointer to a linked list of tabs
	TTabList = record
	  TabIndex:  integer;
	  TabCount:  integer;     //Number of tabs in this list
	  boxHeight: integer;     //All boxes in these tab list are this high
          TabFont:   FontType;
	  TabPos:    PTab;        //Current tab in list
	  TabHead:   PTab;        //Pointer to first tab in this tab list
	  TabTail:   PTab;        //Pointer to last tab in this tab list
end;


Type
	TTabsArray = array [1..10] of PTabList;


  TPostscriptClass = class(TObject)
  private
          fPrinter          : TCUPSPrinter;
	  fTabArray         : TTabsArray;
          fFontArray        : TFontArray;
	  fTabArrayIndex    : Integer;
	  fCurrentX         : Integer;    //Page Cursor
          fCurrentY         : Integer;
	  fCurrentFontName  : String;
	  fCurrentFontSize  : Integer;
          fPSDotsPI         : PrinterDotsPI;
	  fLineScale        : Double;
	  fPrintFileOpen    : Boolean;
	  fPrintFileID      : Text;
	  fPrintFileName    : String;
          fPages            : Integer;
          fPageNo           : Integer;
          fMargins          : PrinterMargins;
          fLineSpace        : Integer;
	  fLineToLine       : Integer;
	  fErrorCode        : Integer;
	  fPageHeight       : integer;
	  fpageWidth        : integer;
          fCurrentFont      : FontType;
          fBold             : Boolean;
          fLandscape        : Boolean;
          fOnLandscapeChange: TNotifyEvent;
          fOnFontChange     : TNotifyEvent;
          fErrorMessage     : String;
	
	 procedure   CreateTabArray;
         procedure   CreateFontArray;
   protected
          property    OnLandscapeChange: TNotifyEvent
                     read fOnLandscapeChange write fOnLandscapeChange;

         procedure   LandscapeChange(Sender: TObject);
         procedure   setLandscape(LS: boolean);
         property    OnFontChange: TNotifyEvent
                     read fOnFontChange write fOnFontChange;

         procedure   setBold(BoldOn: Boolean);
         procedure   setFont(AFont: FontType);
	 procedure   setCurrentX(XLoc: Double);
	 procedure   setCurrentY(YLoc: Double);
	 function    getCurrentX: Double;
	 function    getCurrentY: Double;

	 procedure   setPageHeight(Ln: Double);
	 procedure   setPageWidth( Wd: Double);
         function    getPageHeight: Double;
	 function    getPageWidth: Double;
	 procedure   getPrinterDotsPI;
         function    PrinterDotsToInchX(Pnt: Integer): Double;
         function    PrinterDotsToInchY(Pnt: Integer): Double;
		 
	 procedure   setLineToLine(Spc: Double);
	 function    getLineToLine: Double;
	 procedure   PrintPSPointXY(S: String; XPos, YPos: Integer);
	 function    CalcCenterPage: Integer;

         procedure   PrintPSLeftPoint(S: String; XPos: integer);
	 procedure   PrintPSCenterPoint(S: String; XPos: integer);
	 procedure   PrintPSRightPoint(S: String; XPos: integer);
	 function    NewTabPoint(IDX, XPosition, just, XWidth, XMargin: integer;
                                     TabRel: Boolean; boxLines, boxShade: Integer): PTab;

         procedure   PrintCurrentFont(Sender: TObject);
         function    ComputeFontHeight(FntName: String; FntSize: Integer): Integer;
         function    getFontHeight: Integer;
         function    ShadeToGreyScale(Shade: Integer): TGraphicsColor;
         function    GreyScaleToShade(Shade: TGraphicsColor): Integer;
         function    getPrintFileName: String; //from cupsPrinters.inc
         procedure   RecRoutine;
         procedure   initPage;

   public
	 constructor Create;
	 destructor  Destroy; override;
         procedure   RRect(X1, Y1, X2, Y2, Rad: Integer);  //in points

         function    getBoxShadePercent(TabPtr: PTab): Integer;
 	 function    getBoxShadeString(TabPtr: PTab): String;  //for printing	 function    getFontName(ListPtr: PTabList): String;
         property    PageNo: Integer read fPageNo write fPageNo;

         property    Landscape: Boolean read fLandscape write setLandscape;
         property    PrintFileName: String read fPrintFileName write fPrintFileName;
         property    font: FontType read fCurrentFont write setFont;
         property    LineSpace: integer read fLinespace write fLinespace;
         property    PRDotsPI: PrinterDotsPI read fPSDotsPI;

         property    TabArrayIndex: Integer read fTabArrayIndex write fTabArrayIndex;
	 property    PrintFileOpen: boolean read fPrintFileOpen;
	 property    PrintFileID: Text read fPrintFileID;
	 property    TabArray: TTabsArray read fTabArray;

         property    Bold: Boolean read fBold write setBold;
	 property    LineScale: Double read fLineScale write fLineScale;
	 property    LineSpacing: Double read getLineToLine write setLineToLine;

	 property    CurX: Integer read fCurrentX write fCurrentX;
	 property    CurY: Integer read fCurrentY write fCurrentY;
	 property    CurrentX: Double read getCurrentX write setCurrentX;
	 property    CurrentY: Double read getCurrentY write setCurrentY;

         property    MarginTop:   Integer read fMargins.TopMargin;
         property    MarginLeft:  Integer read fMargins.LeftMargin;
         property    MarginRight: Integer read fMargins.RightMargin;
         property    MarginBottom: Integer read fMargins.BottomMargin;
         procedure   GetPrinterMargins;

         property    PgeNo: Integer read fPageNo write fPageNo;
         property    PageHeight: Double read getPageHeight write setPageHeight;
	 property    PageWidth: Double  read getPageWidth  write setPageWidth;
	 property    PageHeightInt: Integer read fPageHeight;
	 property    PageWidthInt: Integer read fPageWidth; 
	 
	 function    getBoxLeft(Combined: Byte): Boolean;
	 function    getBoxBottom(Combined: Byte): Boolean;
	 function    getBoxRite(Combined: Byte): Boolean;
	 function    getBoxTop(Combined: Byte): Boolean;

	 function    ShadePercentToByte(Percent: integer): Byte;
         function    BoxLinesToByte(Lf, Tp, Rt, Bt: Boolean): Byte;
	 procedure   setBoxWidth(TabPtr: PTab; BWidth: Double);  
	 function    getBoxWidth(TabPtr: PTab):Double;
         procedure   setBoxHeight(ListPtr: PTabList; BHeight: Double);
         function    getBoxHeight(ListPtr: PTabList):Double;

	 function    getTabBoxHeight(IDX: Integer): Integer;
         procedure   setTabBoxHeight(IDX: Integer);


	 procedure   PrintPSXY(S: String; XPos, YPos: Double);
	 procedure   PrintPSLeft(S: String; XPos: Double);
	 procedure   PrintPSCenter(S: String; XPos: Double);
	 procedure   PrintPSRight(S: String; XPos: Double);
	 
	 procedure   PrintPSTab(IDX: integer; S: String);
         function    resetTab(IDX: Integer): PTab;
	 
	 procedure   SaveFontName(IDX: Integer; FName: String);
         procedure   SaveFontSize(IDX, FSize: Integer);
         procedure   RestoreFont(IDX: Integer);
	 procedure   setBoxShade(TabPtr: PTab; Percent: integer);
	 property    FontHeight:Integer read getFontHeight;
         procedure   PutCurrentFont(FName: String; Size: Integer);                   //set current font
	 function    IndexFont(IDX: Integer): Boolean;                                 //Set current font from saved tab array
         procedure   PutTabFont(IDX: Integer; FName: String; Size: Integer);  //set font for tab[IDX]

         function    IndexTabFont(IDX: Integer): Boolean;                              //Set font for tab[IDX] from saved tab array
         function    PToSX(X: Integer): Integer;
         function    PToSY(Y: Integer): Integer;

         function    PointToInch(Pnt: Integer): Double;
	 function    InchToPoint(Inch: Double): Integer;
	 procedure   FreeAllTabs;
	 procedure   FreeTabs(IDX: Integer);
         procedure   FreeFont(IDX: Integer);
         procedure   FreeAllFonts;
	 function    nextTab(IDX: Integer): PTab;
         function    TransArc(Ang: Integer):Integer;
         procedure   Home;

	
    function    calcStringY(Base, Height: Integer): integer;
	//New tab creates a new tab in tabs array width index = IDX
	 function NewTab(IDX: Integer; XPosition: Double; just: Integer;XWidth, XMargin: Double;
	                    TabRel: Boolean; boxLines, boxShade: integer): PTab;
							
     function    EvenTabs(IDX, XPosition, just, XWidth, XMargin, BHeight, Space, Num: integer;
                                    boxLines, boxShade: integer): PTab;
									

	 function    LinesLeft(LineSize: Double): Integer;
	 function    TransXFloat(X: Double): Integer;    //User to x points
	 function    TransYFloat(Y: Double): Integer;    //User to y points
	 function    TransXPoint(X: Integer): Integer;   //user x points to PS points
	 function    TransYPoint(Y: Integer): Integer;   //User y points to PS points
	 procedure   PSNewLine;
         procedure   PSTabNewLine(IDX: Integer);
	 procedure   OpenPrintFile(FileName: String);    // Open file - discriptor goes to fPrintFile
	 procedure   ClosePrintFile;
	 procedure   PrintPSCenterPage(S: String);
	 procedure   setLineScale(Scale: Double);
	 procedure   GotoPSXY(X, Y: Double);
	 //postscript procedures
	 procedure   XLocation(X: Double);
	 procedure   PSProcs;
	 procedure   NewPage;
         procedure   EndPage;
         procedure   ViewFile;
        // procedure   showPage;
  end;

 type
   pAddressRecord = ^TAddressRecord;
   TAddressRecord = record
      AName: String;
      Add1: String;
      Add2: String;
      CityState: String;
      ZipCode: String;
     // PostNetCode: PostNetType;
   end;

 type
  TAddressLabelClass = class(TPostScriptClass)
    private
          fLabelStyle       : String;
          fNumAcross        : integer;
          fNumDown          : integer;
         // fMarginTop        : integer;
          //fMarginLeft       : integer;
          fTextMarginTop    : Integer;
          fTextMarginLeft   : Integer;
          fLabelWidth       : integer;
          fLabelHeight      : integer;
          fRadius           : Integer;
          fSpacingTop       : Integer;
          fSpacingLeft      : Integer;
          fSpacingWidth     : integer;
          fSpacingHeight    : integer;
          fPostNetHeight    : Double;
          fPostNetSpacing   : Double;
          fPostNetLineWidth : Double;
          fPrintPostNet     : Boolean;
          fRowPointer       : Integer;
          fColPointer       : Integer;
          fPageDone         : Boolean;
          fAddressRecord    : TAddressRecord;
          fAddressDataSource: TDataSource;
  protected
    function getRadius: Double;
    procedure setRadius(Rad: Double);
    //function getMarginTop: Double;
    //procedure setMarginTop(Top: Double);
    //function getMarginLeft: Double;
    //procedure setMarginLeft(Left: Double);
    function getTextMarginTop: Double;
    procedure setTextMarginTop(Top: Double);
    function getTextMarginLeft: Double;
    procedure setTextMarginLeft(Left: Double);
    function getLabelWidth: Double;
    procedure setLabelWidth(LabelWidth: Double);
    function getLabelHeight: Double;
    procedure setLabelHeight(LabelHeight: Double);

    function getSpacingTop: Double;
    procedure setSpacingTop(SpacingTop: Double);
    function getSpacingLeft: Double;
    procedure setSpacingLeft(SpacingLeft: Double);
    function getSpacingWidth: Double;
    procedure setSpacingWidth(SpacingWidth: Double);
    function getSpacingHeight: Double;
    procedure setSpacingHeight(SpacingHeight: Double);

    procedure setAddressRecord(Title, fName, lName, Addr1, Addr2, City, State, Zip: String);
  public
    constructor Create;
    procedure initPostnet;
    destructor  Destroy; override;
   // property Address: TAddressRecord  write setAddressRecord;
    property Radius: Double read getRadius write setRadius;
    property LabelStyle: String read fLabelStyle write fLabelStyle;
    property NumAcross: Integer read fNumAcross write fNumAcross;
    property NumDown: Integer read fNumDown write fNumDown;
    //property MarginTop: Double read getMarginTop write setMarginTop;
   // property MarginLeft: Double read getMarginLeft write setMarginLeft;
    property TextMarginTop: Double read getTextMarginTop write setTextMarginTop;
    property TextMarginLeft: Double read getTextMarginLeft write setTextMarginLeft;
    property LabelWidth: Double read getLabelWidth write setLabelWidth;
    property LabelHeight: Double read getLabelHeight write setLabelHeight;
    property SpacingTop: Double read getSpacingTop write setSpacingTop;
    property SpacingLeft: Double read getSpacingLeft write setSpacingLeft;
    property SpacingWidth: Double read getSpacingWidth write setSpacingWidth;
    property SpacingHeight: Double read getSpacingHeight write setSpacingHeight;
    property PostNetHeight: Double read fPostNetHeight write fPostNetHeight;
    property PostNetSpacing: Double read fPostNetSpacing write fPostNetSpacing;
    property PostNetLineWidth: Double read fPostNetLineWidth write fPostNetLineWidth;
    property AddressDataSource: TDataSource read fAddressDataSource write fAddressDataSource;
    property PrintPostNet: Boolean read fPrintPostNet write fPrintPostNet;
    procedure putText(S: String);
    procedure PrintPostNetXY(S: String; X, Y: Integer);
    procedure PrintPSLabels(LabelDataSrc: TDataSource; Outline: Boolean);
    procedure PrintOnePSLabel;
    procedure OutlinePSLabel;
  end;


implementation	
 
 procedure TPostScriptClass.OpenPrintFile(FileName: String);
   var
      H, W: Integer;
   begin
     if fLandscape then
       begin
         H := LETTERWIDTH;
         W := LETTERHEIGHT;
       end
     else
       begin
         H := LETTERHEIGHT;
         W := LETTERWIDTH;
       end;
     assignfile(fPrintFileID, FileName);
     reWrite(fPrintFileID);
	 fPrintFileOpen := True;
     fPrintFileOpen := True;
     writeln(PrintFileID,'%!ps');
     writeln(PrintFileID,'<</PageSize[' + IntToStr(W)+' '+IntToStr(H) +']>>setpagedevice');
     if Assigned(fOnFontChange) then
        OnFontChange(Self);
     psProcs;
   end;

  procedure TPostScriptClass.initPage;
  begin
    getPrinterMargins;
    if PrintFileOpen then
        CloseFile(fPrintFileID);
    fPrintFileOpen := false;
   // fOnLandscapeChange:= nil;
    CurX := 0;
    CurY := 0;
    setLandscape(false);
  end;

  procedure TPostScriptClass.ClosePrintFile;
  begin
    initPage;
  end;
   
 constructor TPostScriptClass.Create;   
    begin
   	inherited create;               //Set defaults
        fPrinter := TCupsPrinter.Create;
        GetPrinterMargins;
        fPageNo := 0;
        fPages := 0;
	fCurrentFont.FontName := HELVETICA;
	fCurrentFont.FontSize := 10;
        fCurrentFont.FontHeight := ComputeFontHeight(HELVETICA,10);
	fLineScale := 1.5;
        fLineSpace := 4;

        fPageHeight := LETTERHEIGHT;             //11.0
	fPageWidth := LETTERWIDTH;              //8.5

        CreateTabArray;                 //create empty tab array
        CreateFontArray;                //Create default Font Array
	fCurrentFont.FontName := HELVETICA;
        fLineToLine := round(fCurrentFont.FontSize * fLineScale);
	fPrintFileName := '';
	fPrintFileOpen := false;

        {CurX := MarginLeft;
        CurY := MarginTop ;  }
        CurX := 0;
        CurY := 0;
        fOnFontChange:= @PrintCurrentFont;
        fOnLandscapeChange:= @LandscapeChange;
        fPageNo:= 1;
        fBold := false;
   end;


  procedure TPostScriptClass.NewPage;
  begin
     if not PrintFileOpen then
        OpenPrintFile(GetPrintFileName);
     writeln(PrintFileID, 'gsave');
     CurrentX := 0.0;
     CurrentY := 0.0;
     CurY := 0;
     CurX := 0;
  end;

  procedure TPostScriptClass.EndPage;
  begin
     if (PrintFileOpen)  then
       begin
         writeln(PrintFileID, 'grestore');
         writeln(PrintFileID, 'showpage');
       end;
  end;


 function TPostScriptClass.LinesLeft(LineSize: Double): Integer;
  var
    PageUsed, LineSizeInt: Integer;
  begin
    LineSizeInt := InchToPoint(LineSize);
    PageUsed := MarginBottom - CurY;
    result := PageUsed div LineSizeInt;
  end;

  procedure TPostScriptClass.setBold(BoldOn: Boolean);
  var
    Dash: Integer;
  begin
     Dash:=pos('-',fCurrentFont.FontName);
     If BoldOn then
       begin
         if Dash <=0 then
           fCurrentFont.FontName:=fCurrentFont.FontName+'-Bold';
       end
      else
        begin
          Dash:=pos('-Bold',fCurrentFont.FontName);
          If Dash > 0 then
             fCurrentFont.FontName := LeftStr(fCurrentFont.FontName,Dash-1);
        end;
       PrintCurrentFont(Self);
  end;

  procedure TPostScriptClass.PrintCurrentFont(Sender: TObject);
  begin
    if (PrintFileOpen)  then
	  begin
	     writeln(PrintFileID,'/',Font.FontName,' findfont');
             writeln(PrintFileID,Font.FontSize,' scalefont');
             writeln(PrintFileID,'setfont');
	   end;
  end;


  function TPostScriptClass.IndexFont(IDX: Integer): Boolean;
  begin
    if fFontArray[IDX]^.FontName = '' then
      begin
        result := false;
        exit;
      end
   else
     result := true;
    If fFontArray[IDX] <> nil then
       begin
         fCurrentFont.FontName := fFontArray[IDX]^.FontName;
         fCurrentFont.FontSize := fFontArray[IDX]^.FontSize;
         fCurrentFont.FontHeight := fFontArray[IDX]^.FontHeight;
       end;
  end;

  function TPostScriptClass.getFontHeight:Integer;
  begin
    result := fCurrentFont.FontHeight;
  end;

 procedure TPostScriptClass.setFont(AFont: FontType);
 begin
   fCurrentFont.fontName := AFont.FontName;
   fCurrentFont.fontSize := AFont.FontSize;
   fCurrentFont.FontHeight := ComputeFontHeight(AFont.FontName, AFont.FontSize);
 end;


   function TPostScriptClass.ComputeFontHeight(FntName: String; FntSize: Integer): Integer;
  var
    JnkCanvas: TPrinterCanvas;
    JnkPrinter: TCupsPrinter;
  begin
    JnkPrinter := TCupsPrinter.Create;
    JnkCanvas := TPrinterCanvas.Create(JnkPrinter);
    JnkCanvas.Font.Name := FntName;
    JnkCanvas.Font.Size:= FntSize;
    result := Abs(JnkCanvas.font.height);
    JnkCanvas.Free;
    JnkPrinter.Free;
  end;


   procedure TPostScriptClass.CreateFontArray; //Array 1..10 of font records
  // Type
  //   PFontType = ^FontType;
  //   FontType = record
  //   FontName: String;
  //   FontSize: Integer;
   var
     IDX: Integer;
     MyP: PFontType;
     P: Pointer;
   begin
      for IDX := 1 to 10 do
        begin
          P := @fFontArray;
          MyP := fFOntArray[IDX];
          fFontArray[IDX]:=new(PFontType);
          MyP := fFOntArray[IDX];
          fFontArray[IDX]^.FontName := HELVETICA;
          fFontArray[IDX]^.FontSize := 10;
         end;
   end;


   procedure  TPostScriptClass.CreateTabArray; //Array 1..10 of tab lists
   var
     IDX: Integer;
	 TabListPtr: PTabList;  
   begin
     for IDX := 1 to 10 do
       begin
	 TabListPtr := new(PTabList);
	 with TabListPtr^ do
	   begin
	     TabIndex := 0;
	     TabPos := nil;
	     TabCount := 0;
	     boxHeight := 0;
	     TabHead := nil;
	     TabTail := nil;
	   end;
     	 fTabArray[IDX] := TabListPtr;
       end;
   end;

 procedure TPostScriptClass.GetPrinterMargins;
 var
    L, T, R, B: Double;
 begin
   getPrinterDotsPI;
   With fPrinter.PaperSize.PaperRect.WorkRect do
     begin
       L := Left / FPSDotsPI.XDotsPI;
       T := Top / FPSDotsPI.YDotsPI;
       R := Right / FPSDotsPI.XDotsPI;
       B := Bottom / FPSDotsPI.YDotsPI;
       fMargins.TopMargin   := Round(Double(T) * POINTS);
       fMargins.LeftMargin  := Round(Double(L) * POINTS);
       fMargins.RightMargin := Round(Double(R) * POINTS);
       fMargins.BottomMargin:= Round(Double(B) * POINTS);
     end;
 end;


   procedure  TPostScriptClass.setTabBoxHeight(IDX: Integer);
   begin
     fTabArray[IDX]^.boxHeight := fTabArray[IDX]^.TabFont.FontHeight + BoxSpace;
   end;

   function   TPostScriptClass.getTabBoxHeight(IDX: Integer): Integer;
   begin
     result :=fTabArray[IDX]^.boxHeight;
   end;


 function TpostScriptClass.EvenTabs(IDX, XPosition, just, XWidth, XMargin, BHeight, Space, Num: integer;
                                    boxLines, boxShade: integer): PTab;
 //A tab list of evenly spaced evenly sized tabs - just a utility									
										
 var
   I, St, Inc, Pos: Integer; 
   TmpTab: PTab;	
  begin
 
  St := XPosition;
  Inc := XWidth + Space;
  Pos := XPosition;
  If (St + Num * Inc) > fPageWidth then
    begin
	  EvenTabs := nil;
	  exit;
	end;  
	
   setTabBoxHeight(IDX);
   fTabArray[IDX]^.TabCount := num;	
  for I := 1 to Num do
    begin
	   TmpTab := NewTabPoint(IDX, Pos, just, XWidth, XMargin, false, BoxLines, BoxShade); 
	   Pos := Pos + Inc;
	   If I = 1 then
	     begin
	       fTabArray[IDX]^.TabHead := TmpTab;
		   fTabArray[IDX]^.TabPos  := TmpTab;
		   fTabArray[IDX]^.TabIndex:= 1;
		 end;  
	end;
	fTabArray[IDX]^.TabTail := TmpTab;
  end;   					
  				
function TPostScriptClass.NewTab(IDX: Integer; XPosition: Double; just: Integer;XWidth, XMargin: Double;
	                    TabRel: Boolean; boxLines, boxShade: integer): PTab;
var
  XP, XW, XM: Integer;
begin
  XP := InchToPoint(XPosition);
  XW := InchToPoint(XWidth);
  XM := InchToPoint(XMargin);
  NewTab := NewTabPoint(IDX, XP,just, XW, XM, TabRel, boxlines, boxshade);
end;								
						    
 function TPostScriptClass.NewTabPoint(IDX, XPosition, just, XWidth, XMargin: integer;
                                  TabRel: Boolean; boxLines, boxShade: integer): PTab;
   // Create a new tab
 var 
    NewPTab: PTab;
	IX: Integer;
	TP: PTab;
 begin
   NewPTab := new(PTab);
   With fTabArray[IDX]^ do
     begin
	   if TabHead = nil then
	     begin
		   NewPTab^.prev := nil;       //First tab in the list
		   TabHead := NewPTab;
		   TabCount := 1;
		   TabIndex := 1;
		   TabPos := NewPTab;
		 end
	   else    
	     begin                         //At least one tab already exists
	       NewPTab^.Prev := TabTail;
		   TabTail^.Next := NewPTab;   //Penultimate tab points to newly created tab
		   TabCount := TabCount + 1;
		 end;  
	   TabTail:= NewPTab;              //The new tab is at the tail of the list ALWAYS
	   
	  // NewPTab^.Next := nil;           //The last tab has no next link	
	    NewPTab^.Next := TabHead;        //Last points to head   	    
     end;
	 
		 
	 With NewPTab^ do                  //Set box parameters, start position and justification 
	   begin 
	     If fTabArray[IDX]^.TabCount = 1 then
		   XPos := XPosition
		 else if (TabRel) and (Prev <> nil) then     
           //New position is relative to prev tab
	       XPos := XPosition + Prev^.XPos + Prev^.BoxWidth
	     else		
           XPos := XPosition;
		   
		 justifyText := just;  
	         BoxWidth := XWidth;
		 Margin := XMargin;
		 BShade := ShadeToGreyScale(BoxShade);
                 PSBShade := boxShade;
		 BLines := boxLines;
	   end;
	 NewTabPoint := NewPTab;  
  end;	   	  
	    	  	  

  procedure   TPostScriptClass.setPageHeight(Ln: Double);
  begin
    fPageHeight := trunc(Ln*POINTS);
  end;
  
  procedure   TPostScriptClass.setPageWidth( Wd: Double);
  begin
    fPageWidth := trunc(Wd*POINTS);
  end;
  
  function    TPostScriptClass.getPageHeight: Double;
  begin
    getPageHeight := Double(fPageHeight)/POINTS;
  end;
  
 function    TPostScriptClass.getPageWidth: Double;
  begin
     getPageWidth := Double(fPageWidth)/POINTS;
  end;
  
  function  TPostScriptClass.ShadePercentToByte(Percent: Integer): Byte;
 var
   B: Byte;
  begin
    If (Percent < 1) then
	  B := 0
	else if (Percent >= 100) then
	  B := 15
	else
	  B := round(Percent/100.0 * 15.0);
	//B := B shl 4;
	ShadePercentToByte := B;

  end;

  function  TPostScriptClass.GreyScaleToShade(Shade: TGraphicsColor): Integer;
 var
   Intensity: Integer;
begin
   Intensity := Shade shr 4;
   if Intensity >= 255 then
      result := 0
   else if Intensity <= 0 then
      result := 10
   else
      result := 255 - intensity;

end;

  //scale 1 to 100 - percent
  function  TPostScriptClass.ShadeToGreyScale(Shade: Integer): TGraphicsColor;
  var
  red, blue, green: Integer;   //0 is black, 255 white   $FF
begin
   If (Shade <= 0) then
     Red := 255
   else if (Shade >= 100) then
     Red := 0
   else
     Red := 255 - trunc(255.0 / 100.0 * Shade);

   Green := Red shl 8;
   Blue := Red shl 16;
   result := Red or Green or Blue;
 end;



 function TPostScriptClass.getBoxShadePercent(TabPtr: PTab): Integer;
   begin
     result := GreyScaleToShade(TabPtr^.BShade);
   end;

 procedure  TPostScriptClass.setBoxShade(TabPtr: PTab; Percent: integer);
 begin
    TabPtr^.PSBShade := ShadePercentToByte(Percent);
    TabPtr^.BShade := ShadeToGreyScale(Percent);;
 end;
  
 function  TPostScriptClass.getBoxShadeString(TabPtr: PTab): String;
  begin
    //20.0 should be 15.0 but grays tend to be too dark
    getBoxShadeString := FloatToStrF(1.0 - TabPtr^.PSBShade/20.0, ffFixed, 3, 1);
  end;
  
  
  function  TPostScriptClass.BoxLinesToByte(Lf, Tp, Rt, Bt: Boolean): Byte;
   var
    Res: Byte;
  begin
    Res:=0;
	If Lf then Res := Res or BOXLINELEFT;
	If Tp then Res := Res or BOXLINETOP;
	If Rt then Res := Res or BOXLINERIGHT;
	If Bt then Res := Res or BOXLINEBOTTOM;
	BoxLinesToByte := Res;
  end;

  procedure TPostScriptClass.SaveFontName(IDX: Integer; FName: String);
  // Type
  //   PFontType = ^FontType;
  //   FontType = record
  //   FontName: String;
  //   FontSize: Integer;
   var
     MyP: PFontType;
     P: Pointer;
  begin
    P := @fFOntArray;
    P := fFOntArray[IDX];
    if fFontArray[IDX] <> nil then
      fFontArray[IDX]^.FontName := FName;
  end;

  procedure TPostScriptClass.SaveFontSize(IDX, FSize: Integer);
  begin
    if fFontArray[IDX] <> nil then
      fFontArray[IDX]^.FontSize := FSize;
  end;

  procedure TPostScriptClass.RestoreFont(IDX: Integer);
  var
    TmpFont: FontType;
  begin
    TmpFont.FontName := fFontArray[IDX]^.FontName;
    TmpFont.FontSize := fFontArray[IDX]^.FontSize;
    Font := TmpFont;
  end;


  procedure TPostScriptClass.PutCurrentFont(FName: String; Size: Integer);
  begin
      begin
        fCurrentFont.FontName := FName;
        fCurrentFont.FontSize := Size;
        fCurrentFont.FontHeight := ComputeFontHeight(FName, Size);
      end;
  end;

  procedure TPostScriptClass.PutTabFont(IDX: Integer; FName: String; Size: Integer);
  begin
      begin
        fTabArray[IDX]^.TabFont.FontName := FName;
        fTabArray[IDX]^.TabFont.FontSize := Size;
        fTabArray[IDX]^.TabFont.FontHeight := ComputeFontHeight(FName, Size);
        setTabBoxHeight(IDX);
      end;
  end;

  function TPostScriptClass.IndexTabFont(IDX: Integer):Boolean;
  begin
    if fFontArray[IDX]^.FontName = '' then
      begin
        result := false;
        exit;
      end
   else
     result := true;

    With fTabArray[IDX]^.TabFont do
      begin
        FontName := fFontArray[IDX]^.FontName;
        FontSize := fFontArray[IDX]^.FontSize;
        FontHeight := fFontArray[IDX]^.FontHeight;
      end;
  end;

  procedure TPostScriptClass.Home;
  begin
    CurX := MarginLeft;
    CurY := MarginTop;
  end;

function TPostScriptClass.PToSX(X: Integer): Integer;
begin
   With fPrinter do
     result := trunc(X / PRDotsPI.XDotsPI * 72.0)
end;

function TPostScriptClass.PToSY(Y: Integer): Integer;
begin
   With fPrinter do
     result := trunc(Y / PRDotsPI.YDotsPI * 72.0)
end;

  function TPostScriptClass.getBoxLeft(Combined: Byte): boolean;
  begin  
    getBoxLeft:=(Combined and 1) > 0;   
  end;	    
 
 function TPostScriptClass.getBoxBottom(Combined: Byte): boolean;
  begin  
    getBoxBottom:=(Combined and 8) > 0;   
  end;	  
  
  function TPostScriptClass.getBoxRite(Combined: Byte): boolean;
  begin  
     getBoxRite:=(Combined and 4) > 0;   
  end;	  
  
  function TPostScriptClass.getBoxTop(Combined: Byte): boolean;
  begin  
    getBoxTop:=(Combined and 2) > 0;   
  end;	  

  procedure TPostScriptClass.setBoxWidth(TabPtr: PTab; BWidth: Double);
  begin
    TabPtr^.BoxWidth := InchToPoint(BWidth);
  end;


  function  TPostScriptClass.getBoxWidth(TabPtr: PTab):Double;
  begin
    getBoxWidth := PointToInch(TabPtr^.BoxWidth);
  end;


  procedure TPostScriptClass.setBoxHeight(ListPtr: PTabList; BHeight: Double);
  begin
    ListPtr^.BoxHeight := InchToPoint(BHeight);
  end;

  function  TPostScriptClass.getBoxHeight(ListPtr: PTabList):Double;
  begin
    getBoxHeight := PointToInch(ListPtr^.BoxHeight);
  end;

  function  TPostScriptClass.InchToPoint(Inch: Double): Integer;
  begin
    InchToPoint := round(Inch * POINTS);
  end;
  
 function   TPostScriptClass.PointToInch(Pnt: Integer): Double;
   begin
      PointToInch := Double(Pnt)/POINTS;
   end;
  	

  procedure TPostScriptClass.setCurrentX(XLoc: Double);
  begin
    fCurrentX := round(XLoc*POINTS) + MarginLeft;
  end; 
  
  function  TPostScriptClass.getCurrentX: Double;
  begin
    getCurrentX := Double(fCurrentX)/POINTS - MarginLeft;
  end;
  
  
  procedure TPostScriptClass.setCurrentY(YLoc: Double);
  begin
    fCurrentY := round(YLoc*POINTS) + MarginTop;
  end; 
  
  function  TPostScriptClass.getCurrentY: Double;
  begin
    getCurrentY := Double(fCurrentY)/POINTS - MarginTop;
  end;
  
  procedure  TPostScriptClass.PrintPSTab(IDX: Integer; S: String);
  var
    YPos, TmpY: Integer;
	Shade: String;
	just, SY, Box, XStart, FH: integer;
	BoxBase, BoxLeft, BoxTop, BoxRight, BoxHght, BoxWdth, Marg: Integer;
	TabPtr: PTab;
	FN: String;
  begin
    if not PrintFileOpen then exit;
    if (IDX <= 0) or (IDX > 10) then exit;
	YPos := CurY; 
	TabPtr := fTabArray[IDX]^.TabPos;
	Shade := getBoxShadeString(TabPtr);
	With fTabArray[IDX]^ do
	  begin
	    BoxHght := fTabArray[IDX]^.boxHeight;
	    //Box height is common to all tabs in this list
	    SY := calcStringY(YPos,BoxHght);  //Y location for string inside the box
	    BoxBase := TransYPoint(YPos);     //Base line of box is at YPos
          end;
	With TabPtr^ do
	  begin
	    Marg := Margin;
	    BoxLeft := TransXPoint(Xpos);  //WRiteln('BoxLeft ', BoxLeft,' ',XPos);           
	    BoxRight := TransXPoint(XPos + BoxWidth);
	    BoxWdth := BoxWidth;
	    BoxTop := TransYPoint(YPos - BoxHght);  //Measuring Y = 0 at top of page
	    //BoxTop := BoxBase + BoxHght;          //could eliminate one function call
	    just := justifyText;
	    Box := BLines;
	  end;			
	
	 //fill tab box
	 writeln(PrintFileID,Shade,  ' setgray');
	 writeln(PrintFileID,'newpath');
	 writeln(PrintFileID,BoxLeft,' ',BoxBase,' moveto');
	 writeln(PrintFileID,BoxWdth,' ',0,' rlineto');
	 writeln(PrintFileID,0,' ',BoxHght,' rlineto');
	 writeln(PrintFileID,-BoxWdth,' ',0,' rlineto');
	 writeln(PrintFileID,'closepath');
	 writeln(PrintFileID,'fill');
         writeln(PrintFileID, '0.0 setgray');

	 //box lines if any
	 if getBoxBottom(Box) then
	    begin
	      writeln(PrintFileID,BoxLeft,' ',BoxBase,' moveto');
	      writeln(PrintFileID,BoxWdth,' ',0,' rlineto');
		  writeln(PrintFileID,'stroke');
		end;
	  if getBoxRite(Box) then
	    begin	
		  writeln(PrintFileID,BoxRight,' ',BoxBase,' moveto');
	      writeln(PrintFileID,0,' ',BoxHght,' rlineto');
		  writeln(PrintFileID,'stroke');
		end;
	  if getBoxTop(Box) then
	    begin	
		   writeln(PrintFileID,BoxLeft,' ',BoxTop,' moveto');
	       writeln(PrintFileID,BoxWdth,' ',0,' rlineto');
		   writeln(PrintFileID,'stroke');
		end;
	   if getBoxLeft(Box) then
	     begin	  
	       writeln(PrintFileID,BoxLeft,' ',BoxBase,' moveto');
	       writeln(PrintFileID,0,' ',BoxHght,' rlineto');
	       writeln(PrintFileID,'stroke');
	     end;
	   if S <> '' then
	     begin
	       TmpY := CurY;
	       CurY := SY;
	       if just = JUSTIFYRIGHT then
	         begin
		   XStart := BoxRight - Font.Fontsize div 2;
		   PrintPSRightPoint(S, XStart);
		 end
	       else if just = JUSTIFYCENTER then
	         begin
	           XStart := BoxLeft + BoxWdth div 2;
		   PrintPSCenterPoint(S,XStart);
	         end
	       else
	         begin
		   PrintPSLeftPoint(S, BoxLeft + Marg);
		 end;
	       CurY := TmpY;
	     end;  //if S <>
           nextTab(IDX);
  end;
  
 procedure TPostScriptClass.PrintPSPointXY(S: String; XPos, YPos: Integer);
 //Print a string at X & Y without altering CurY
 begin
  if (not PrintFileOpen) then exit;
    writeln(PrintFileID,'0.0 setgray');
    writeln(PrintFileID,XPos,' ',YPos,' moveto');
	writeln(PrintFileID,'(',S,')',' show');
 end;
 

  function  TPostScriptClass.ResetTab(IDX: Integer): PTab;
  begin
    //With TabListPtr^ do
    With fTabArray[IDX]^ do
       begin
         TabPos := TabHead;
	 TabIndex := 1;
         ResetTab := TabPos;
       end;
  end;

  //function  TPostScriptClass.nextTab(TabListPtr: PTabList): PTab;
  function  TPostScriptClass.nextTab(IDX: Integer): PTab;
  begin
    With fTabArray[IDX]^ do
	    begin
  		  If TabPos = TabTail then  //Last tab? then wrap to first
		    begin
		      TabPos := TabHead;
		      TabIndex := 1;
                      PSTabNewLine(IDX);             //Automatic newline after last tab
	 	   end
		  else
		    begin
		      TabPos := TabPos^.next;	  //increment tab position	
			  TabIndex := TabIndex + 1;
			  end; 
          nextTab := TabPos;		
		end;	
		
  end;
  	
  function TPostScriptClass.calcStringY(Base, Height: Integer): integer;
  var
    Margin, FSZ: integer;
  begin
 	FSZ := round(Double(Font.FontSize) * 0.75);
	if FSZ < Height then
	  begin
	    Margin := (Height - FSZ ) div 2 + 1;
	    calcStringY := Base - Margin;
	  end 	
	else
	  calcStringY := Base - 3;
  end;
  					
  procedure TPostScriptClass.FreeAllTabs;
  var
    I: Integer;
  begin
    for I := 1 to 10 do
	  FreeTabs(I);
  end;	  	

 procedure TPostScriptClass.FreeAllFonts;
  var
    I: Integer;
  begin
    for I := 1 to 10 do
	  FreeFont(I);
  end;

  procedure TPostScriptClass.FreeFont(IDX: Integer);
  var
    FontPtr: PFontType;
  begin
    dispose(fFontArray[IDX]);
  end;

  procedure TPostScriptClass.FreeTabs(IDX: Integer);
  var
    ClrTab, NuTab, LastTab: PTab;
  begin
    if (IDX <= 0) then exit;
    ClrTab:=TabArray[IDX]^.TabHead;
    LastTab := TabArray[IDX]^.TabTail;
    if ClrTab <> nil then
    while (ClrTab <> LastTab) do
      begin
        NuTab:=ClrTab^.Next;
        dispose(ClrTab);
        ClrTab := NuTab;
      end;
    dispose(clrTab);
	
    With TabArray[IDX]^ do
      begin
        IDX := 0;
        TabPos := nil;
        TabCount := 0;
        boxHeight := 0;
        TabHead := nil;
        TabTail := nil;
      end;
end;	 	

 procedure TPostScriptClass.PrintPSXY(S: String; XPos, YPos: Double);
 //Print a string at X & Y without altering CurY
 var
   X, Y: Integer;
 begin
   if fPrintFileOpen then
     begin
       X := TransXFloat(XPos);
       Y := TransYFloat(YPos);
       PrintPSPointXY(S, X, Y);
     end;
 end;
 
 procedure TPostScriptClass.GotoPSXY(X, Y: Double);
 var
   XInt, YInt: Integer;
 begin
   XInt := TransXFloat(X);
   YInt := TransYFloat(Y);
   writeln(PrintFileID, XInt,' ',Yint,' moveto');
   CurY := YInt;
  end;
 
 procedure TPostScriptClass.PrintPSCenterPage(S: String);
 var
   X: Integer;
   Cstr: String;
 begin
   Cstr := '('+S+') centershow';
   X := CalcCenterPage;
   writeln(PrintFileID,X,' ',TransYPoint(CurY),' moveto');
   writeln(PrintFileID,Cstr);
  end;

 function TPostScriptClass.CalcCenterPage: Integer;
 begin
   CalcCenterPage := (MarginRight- MarginLeft) div 2 + MarginLeft;
 end;
  
  
 procedure TPostScriptClass.PrintPSLeft(S: String; XPos: Double);
 begin
   writeln(PrintFileID,TransXFloat(XPos),' ',TransYPoint(CurY),' moveto');
   writeln(PrintFileID,'(',S,')', ' show');
 end;
 
 procedure TPostScriptClass.PrintPSCenter(S: String; XPos: Double);
 var
   X: Integer;
   Cstr: String;
 begin
   Cstr := '('+S+') centershow';
   X := TransXFloat(XPos);
   writeln(PrintFileID,X,' ',TransYPoint(CurY),' moveto');
   writeln(PrintFileID,Cstr);
 end;
  
 procedure TPostScriptClass.PrintPSRight(S: String; XPos: Double);
 var
   X: Integer;
   Cstr: String;
 begin 
   X := TransXFloat(XPos);
   writeln(PrintFileID,X,' ',TransYPoint(fCurrentY),' moveto');
   Cstr := '('+S+') rightshow';
   //writeln(PrintFileID,Cstr);
 end;
  
  procedure TPostScriptClass.PrintPSLeftPoint(S: String; XPos: integer);
 begin
   writeln(PrintFileID,XPos,' ',TransYPoint(CurY),' moveto');
   writeln(PrintFileID,'(',S,')', ' show');
 end;
 
 procedure TPostScriptClass.PrintPSCenterPoint(S: String; XPos: integer);
  begin 
   writeln(PrintFileID,XPos,' ',TransYPoint(CurY),' moveto');
   writeln(PrintFileID,'(',S,') centershow');
 end;
  
 procedure TPostScriptClass.PrintPSRightPoint(S: String; XPos: integer);
 begin 
    writeln(PrintFileID,XPos,' ',TransYPoint(fCurrentY),' moveto');
   writeln(PrintFileID,'(',S,') rightshow');
 end;


  procedure TPostScriptClass.setLineToLine(Spc: Double);
  begin
    fLineToLine := round(Double(fCurrentFontSize) * fLineScale);
  end;
  	
  function TPostScriptClass.getLineToLine: Double;
  begin
    getLineToLine := PointToInch(fLineToLine);
  end;
  

  procedure  TPostScriptClass.getPrinterDotsPI;
  begin
    fPSDotsPI.XDotsPI := fPrinter.XDPI;
    fPSDotsPI.YDotsPI := fPrinter.YDPI;
  end;

  function  TPostScriptClass.PrinterDotsToInchX(Pnt: Integer): Double;
  begin
    Result := Double(Pnt ) / fPSDotsPI.XDotsPI;
  end;

  function  TPostScriptClass.PrinterDotsToInchY(Pnt: Integer): Double;
  begin
    Result := Double(Pnt ) / fPSDotsPI.YDotsPI;
  end;
 { function TPostScriptClass.LinesLeft: Integer;
  var
    PageUsed: Integer;	
  begin
    PageUsed := fPageLength - CurY - fBottomMargin;
	LinesLeft := PageUsed div fLineToLine; 
  end;}
   
  function TPostScriptClass.TransXFloat(X: Double): Integer;
  begin
    TransXFloat := TransXPoint(InchToPoint(X));
  end;

  function TPostScriptClass.TransArc(Ang: Integer):Integer;
  begin
    if Ang = 270 then
      result := 90
    else if Ang = 90 then
       result := 270
    else
       result := Ang;
  end;

  function TPostScriptClass.TransYFloat(Y: Double): Integer;
  begin
    TransYFloat := TransYPoint(InchToPoint(Y));
  end;
  
  function TPostScriptClass.TransXPoint(X: Integer): Integer;
  begin
    TransXPoint := X + MarginLeft;
  end; 
  	
  function TPostScriptClass.TransYPoint(Y: Integer): Integer; 
  begin
    TransYPoint := fPageHeight - Y; //fPageLength - MarginTop - Y;
   end;	

procedure TPostScriptClass.XLocation(X: Double);
var
  Xint: Integer;
begin
 if (PrintFileOpen)  then
   begin
     XInt := InchToPoint(X);
 	 writeln(PrintFileID,'/loc ',Xint,' def');
   end; 
 // /loc Xint def
end;
 
procedure TPostScriptClass.psProcs;
begin
  if (PrintFileOpen)  then
    begin
	  writeln(PrintFileID,'/rightshow');
	  writeln(PrintFileID,'{dup stringwidth pop');
	  writeln(PrintFileID,'0 exch sub');
	  writeln(PrintFileID,'0 rmoveto');
	  writeln(PrintFileID,'show} def');
	  writeln(PrintFileID);
	  
	  writeln(PrintFileID,'/centershow');
	  writeln(PrintFileID,'{dup stringwidth pop');
	  writeln(PrintFileID,'2 div');
	  writeln(PrintFileID,'0 exch sub');
	  writeln(PrintFileID,'0 rmoveto');
	  writeln(PrintFileID,'show} def');
	  writeln(PrintFileID);

          RecRoutine;
	 end; 
end;

  procedure TPostScriptClass.setLineScale(Scale: Double);
  begin
    fLineScale := Scale;
  end;

  procedure TPostScriptClass.PSTabNewLine(IDX: Integer);
  begin
    CurY := CurY + fTabArray[IDX]^.boxHeight;
   // if  (BOXLINEBOTTOM) and (fTabArray[IDX]^.TabPos^.BLines)> 0
    //  then CurY := CurY + 1;
  end;

  procedure TPostScriptClass.PSNewLine;
  begin
    CurY := CurY + Round(Font.FontSize * LineScale) + 1;
   // CurY := CurY + InchToPoint(LineSpacing);//fLineToLine;
  end;
     
  destructor TPostScriptClass.Destroy;
  begin
  // InitCriticalSection(fCriticalSection); 
	FreeAllTabs;
        FreeAllFonts;
        fPrinter.destroy;
	inherited Destroy;
		
	//DoneCriticalSection(fCriticalSection);	
end;

 procedure TPostScriptClass.RecRoutine;
 begin
   if PrintFileOpen then
     begin
       writeln(PrintFileID,'/RRect {');
       writeln(PrintFileID,'5 dict begin');
       writeln(PrintFileID,'/radius exch def');
       writeln(PrintFileID,'/y2 exch def');
       writeln(PrintFileID,'/x2 exch def');
       writeln(PrintFileID,'/y1 exch def');
       writeln(PrintFileID,'/x1 exch def');
       writeln(PrintFileID,'gsave');
       writeln(PrintFileID,'newpath');
       writeln(PrintFileID,'x1 radius add y1 radius add radius 180 270 arc');
       writeln(PrintFileID,'x2 radius sub y1 radius add radius 270 0 arc');
       writeln(PrintFileID,'x2 radius sub y2 radius sub radius 0 90 arc');
       writeln(PrintFileID,'x1 radius add y2 radius sub radius 90 180 arc');
       writeln(PrintFileID,'closepath');
       writeln(PrintFileID,'stroke');
       writeln(PrintFileID,'grestore');
       writeln(PrintFileID,'end');
       writeln(PrintFileID,'}');
       writeln(PrintFileID,'def');
       writeln(PrintFileID);
     end;
 end;

 procedure TPostScriptClass.setLandscape(LS: boolean);
begin
  fLandscape := LS;
  if Assigned(fOnLandscapeChange) then
        OnLandscapeChange(Self);
end;


procedure doSwap(var i,j: integer);
var
    tmp: Integer;
begin
    tmp := i;
    i := j;
    j := tmp;
end;

procedure TPostScriptClass.LandscapeChange(Sender: TObject);
var
     H, W: Integer;
begin
  getPrinterMargins;
  if fLandscape then
    begin
      H := LETTERWIDTH;
      W := LETTERHEIGHT;
    end
  else
    begin
      H := LETTERHEIGHT;
      W := LETTERWIDTH;
    end;
  if fLandscape then
    begin
      fPageHeight := LETTERWIDTH;             //11.0
      fPageWidth :=  LETTERHEIGHT;
      if fMargins.bottomMargin > fMargins.rightMargin then
        doswap(fMargins.rightMargin, fMargins.bottomMargin);
    end
  else
    begin
      fPageHeight := LETTERHEIGHT;             //11.0
      fPageWidth :=  LETTERWIDTH;
      if fMargins.bottomMargin < fMargins.rightMargin then
         doswap(fMargins.rightMargin, fMargins.bottomMargin);
     end;
end;

 procedure TPostScriptClass.RRect(X1, Y1, X2, Y2, Rad: Integer);  //in points
 var
   XS, YS, XS2, YS2, RA: String;
 begin
     XS := IntToStr(X1);
     YS2 := IntToStr(TransYPoint(Y1));
     XS2 := IntToStr(X2);
     YS := IntToStr(TransYPoint(Y2));
     RA := IntToStr(Rad);
    if PrintFileOpen then
      writeln(PrintFileID,XS+' '+ YS+' '+ XS2+' '+ YS2+' '+ RA +' RRect');
 end;

   constructor TAddressLabelClass.Create;
    begin
      inherited create;
      fRowPointer := 0;
      fColPointer := 0;
      fPageDone := False;
      fPostNetHeight := 0.125;
      fPostNetSpacing := 0.04;
      fPostNetLineWidth := 0.15;
    end;

  destructor TAddressLabelClass.Destroy;
  begin
  // InitCriticalSection(fCriticalSection);
  inherited Destroy;
  //DoneCriticalSection(fCriticalSection);
end;

   procedure TAddressLabelClass.setAddressRecord(Title, fName, lName,
                                Addr1, Addr2, City, State, Zip: String);
     begin
       fAddressRecord.AName := Title+' '+fName+' '+ lName;
       fAddressRecord.Add1 := Addr1;
       fAddressRecord.Add2 := Addr2;
       fAddressRecord.CityState:= City+','+State;
       fAddressRecord.ZipCode := Zip;
     end;


   function TAddressLabelClass.getRadius: Double;
   begin
     result := PointToInch(fRadius);
   end;

   procedure TAddressLabelClass.setRadius(Rad: Double);
   begin
     fRadius := InchToPoint(Rad);
   end;

   { function TAddressLabelClass.getMarginTop: Double;
    begin
      result := PointToInch(MarginTop);
    end;

    procedure TAddressLabelClass.setMarginTop(Top: Double);
    begin
       MarginTop := InchToPoint(Top);
    end;

    function TAddressLabelClass.getMarginLeft: Double;
    begin
      result := PointToInch(MarginLeft);
    end;

    procedure TAddressLabelClass.setMarginLeft(Left: Double);
    begin
      MarginLeft := InchToPoint(Left);
    end;   }

    function TAddressLabelClass.getTextMarginTop: Double;
    begin
      result := PointToInch(fTextMarginTop);
    end;

    procedure TAddressLabelClass.setTextMarginTop(Top: Double);
    begin
       fTextMarginTop := InchToPoint(Top);
    end;

    function TAddressLabelClass.getTextMarginLeft: Double;
    begin
      result := PointToInch(fTextMarginLeft);
    end;

    procedure TAddressLabelClass.setTextMarginLeft(Left: Double);
    begin
      fTextMarginLeft := InchToPoint(Left);
    end;

    function TAddressLabelClass.getLabelWidth: Double;
    begin
      result := PointToInch(fLabelWidth);
    end;

    procedure TAddressLabelClass.setLabelWidth(LabelWidth: Double);
    begin
      fLabelWidth := InchToPoint(LabelWidth);
    end;

    function TAddressLabelClass.getLabelHeight: Double;
    begin
      result := PointToInch(fLabelHeight);
    end;

    procedure TAddressLabelClass.setLabelHeight(LabelHeight: Double);
    begin
      fLabelHeight := InchToPoint(LabelHeight);
    end;

    function TAddressLabelClass.getSpacingWidth: Double;
    begin
      result := PointToInch(fSpacingWidth);
    end;

    procedure TAddressLabelClass.setSpacingLeft(SpacingLeft: Double);
    begin
      fSPacingLeft := InchToPoint(SpacingLeft);
    end;

    function TAddressLabelClass.getSpacingLeft: Double;
    begin
      result := PointToInch(fSpacingLeft);
    end;

     procedure TAddressLabelClass.setSpacingTop(SpacingTop: Double);
    begin
      fSPacingTop := InchToPoint(SpacingTop);
    end;

    function TAddressLabelClass.getSpacingTop: Double;
    begin
      result := PointToInch(fSpacingTop);
    end;

    procedure TAddressLabelClass.setSpacingWidth(SpacingWidth: Double);
    begin
      fSPacingWidth := InchToPoint(SpacingWidth);
    end;

    function TAddressLabelClass.getSpacingHeight: Double;
    begin
      result := PointToInch(fSpacingHeight);
    end;

    procedure TAddressLabelClass.setSpacingHeight(SpacingHeight: Double);
    begin
       fSPacingHeight := InchToPoint(SpacingHeight);
    end;

   procedure TAddressLabelClass.PrintOnePSLabel;
   var
     X, Y: Integer;
    YAdd1, YAdd2, YCSZ,YPostNet: Integer;
   begin
      X := fSpacingLeft + fTextMarginLeft + fColPointer * (fSpacingWidth );
      Y := fPageHeight - fTextMarginTop  - fSpacingTop
                       - fRowPointer * (fSpacingHeight);
     { X := MarginLeft + fTextMarginLeft + fColPointer * (fSpacingWidth );
      Y := fPageLength - MarginTop  - fTextMarginTop
                       - fRowPointer * (fSpacingHeight);  }
      YAdd1 := Y - fLineToLine;
      if fAddressRecord.Add2 <> '' then
         YAdd2 := YAdd1 - fLineToLine
      else
         YAdd2 := YAdd1;
      YCSZ  :=  YAdd2 - fLineToLine;

      With fAddressRecord do
        begin
          PrintPSPointXY(AName, X, Y);
          PrintPSPointXY(Add1, X, YAdd1);
          if Add2 <> '' then
          PrintPSPointXY(Add2, X, YAdd2);
          PrintPSPointXY(CityState+' '+ZipCode, X, YCSZ);
        end;
      if fPrintPostNet then
        begin
         { YPostNet := fPageLength - MarginTop - fLabelHeight + fTextMarginTop -
                       fRowPointer * (fSpacingHeight);  }
         YPostNet := fPageHeight - fSpacingTop - fLabelHeight + fTextMarginTop -
                       fRowPointer * (fSpacingHeight);

          PrintPostNetXY(faddressRecord.ZipCode, X+2, YPostNet);
        end;
      inc(fColPointer);
      if fColPointer = fNumAcross then
        begin
          fColPointer := 0;
          inc(fRowPointer);
          if FRowPointer = fNumDown then
            begin
              fRowPointer := 0;
              EndPage;
              fPageDone := true;
            end;
        end;
   end;


   procedure TAddressLabelClass.PrintPSLabels(LabelDataSrc: TDataSource; Outline: Boolean);
    begin
      if not PrintFileOpen then
        OpenPrintFile(GetPrintFileName);
   // DataSet :=
    if fPrintPostNet then  initPostnet;
    With LabelDataSrc.DataSet do
      begin
        if not active then open;
        first;
        Newpage;
        if OutLine then OutlinePSLabel;
        while not EOF do
          begin
            fPageDone := False;
            fAddressRecord.AName := FieldByName('TITLE').AsString + ' ' +
                                    FieldByName('FNAME').AsString + ' ' +
                                    FieldByName('NAME').AsString;
             fAddressRecord.Add1 := FieldByName('ADDRESS_1').AsString;
             fAddressRecord.Add2 := FieldByName('ADDRESS_2').AsString;
             fAddressRecord.CityState    := FieldByName('CITY').AsString + ',' +
                                    FieldByName('STATE').AsString;
             fAddressRecord.ZipCode      := FieldByName('ZIP').AsString;
             PrintOnePSLabel;
             if fPageDone then
               begin
                 Newpage;
                 if Outline then OutlinePSLabel;
                 fPageDone := false;
               end;
             next;
          end; //While not EOF
      end; //With DataSet
      if fPageDone then
          fPageDone := false;
      endPage;
    ClosePrintFile;
 end;

   procedure TAddressLabelClass.OutlinePSLabel;
   var
     X0, Y0, X, Y, Rad: Integer;
     Across, Down: Integer;
   begin
     X0 := fSpacingLeft; // - MarginLeft;
     Y0 := fSpacingTop; // - MarginTop;

     for Across := 0 to fNumAcross - 1 do
        for Down := 0 to fNumDown - 1 do
          begin
            X := X0 + Across * fSpacingWidth;
            Y := Y0 + Down * fSpacingHeight;
            RRect(X, Y , X + fLabelWidth, Y + fLabelHeight, fRadius);
          end;
   end;



   procedure TAddressLabelClass.putText(S: String);
   begin
     writeln(PrintFileID,S);
   end;

   procedure TAddressLabelClass.PrintPostNetXY(S: String; X, Y: Integer);
   begin
     writeln(PrintFileID,IntToStr(X)+' '+IntToStr(Y)+' '+'moveto');
     writeln(PrintFileID,'('+S+')'+' barshow');
           // example 72 576 moveto
      //(164331115) barshow
   end;

   procedure TAddressLabelClass.initPostnet;
    var
      SmBarHt, BarHt, BarSpace: String;
    begin
      if not PrintFileOpen then exit;
      writeln(PrintFileID, '0.0 setgray');   // black bar code
      BarHt := FloatToStr(fPostNetHeight);
      SmBarHt := FloatToStr(fPostNetHeight / 2.0);
      BarSpace := FloatToStr(fPostNetSpacing);
      putText(FloatToStr(fPostNetLineWidth)+' setlinewidth');
      putText('/inch {72 mul} def');
      putText('/tall '+BarHt+' inch def');
      putText('/short '+SmBarHt+' inch def');
      putText('/space '+BarSpace+' inch def');
      putText('/frame {0.0 tall rlineto  space tall neg rmoveto} def');
      putText('/drawtall {0.0 tall rlineto  space tall neg rmoveto} def ');
      putText('/drawshort {0.0 short rlineto  space short neg rmoveto} def');
      putText('/one {drawshort drawshort drawshort drawtall drawtall} def ');
      putText('/two {drawshort drawshort drawtall drawshort drawtall} def');
      putText('/three { drawshort drawshort drawtall drawtall drawshort} def');
      putText('/four {drawshort drawtall drawshort drawshort drawtall} def');
      putText('/five {drawshort drawtall drawshort drawtall drawshort} def');
      putText('/six {drawshort drawtall drawtall drawshort drawshort} def');
      putText('/seven {drawtall drawshort drawshort drawshort drawtall} def');
      putText('/eight {drawtall drawshort drawshort drawtall drawshort} def');
      putText('/nine {drawtall drawshort drawtall drawshort drawshort} def');
      putText('/zero {drawtall drawtall drawshort drawshort drawshort} def');
      putText('/sum 0 def');
      putText('/barshow');
      putText('{ frame');
      putText('/thestring exch def');
      putText('thestring');
      putText('{ /charcode exch def');
      putText('48 charcode  eq { ');
      putText('  zero');
      putText('}{');
      putText('} ifelse');
      putText('49 charcode  eq { ');
      putText('  one');
      putText('}{');
      putText('} ifelse');
      putText('50 charcode  eq { ');
      putText('  two');
      putText('}{');
      putText('} ifelse');
      putText('51 charcode  eq {');
      putText('  three');
      putText('}{');
      putText('} ifelse');
      putText('52 charcode  eq {');
      putText('  four');
      putText('}{');
      putText('} ifelse');
      putText('53 charcode  eq {');
      putText('   five');
      putText('}{');
      putText('} ifelse');
      putText(' 54 charcode  eq {');
      putText('  six');
      putText('}{ ');
      putText('} ifelse');
      putText(' 55 charcode  eq { ');
      putText('  seven  ');
      putText('}{');
      putText('} ifelse');
      putText(' 56 charcode  eq {');
      putText('  eight');
      putText('}{');
      putText('} ifelse');
      putText(' 57 charcode  eq {');
      putText('  nine');
      putText('}{');
      putText('} ifelse');
      putText('/thechar ( ) dup 0 charcode put def');
      putText('} forall');
      putText('frame');
      putText('stroke');
      putText('} def');
      // example 72 576 moveto
      //(164331115) barshow
   end;

//PrintFile(aFileName: String): longint;   -1 is error
function TPostScriptClass.getPrintFileName: String; //from cupsPrinters.inc
var
  NewPath: String;

  function TryTemporaryPath(const Path: string): Boolean;
  var
    CurPath: String;
  begin
    CurPath:=CleanAndExpandDirectory(Path);
    Result:=DirPathExists(CurPath);
    if Result then NewPath:=CurPath;
  end;

begin

  if (not TryTemporaryPath('~/tmp/'))
  and (not TryTemporaryPath('/tmp/'))
  and (not TryTemporaryPath('/var/tmp/')) then
    NewPath:='';

  fPrintFileName := AppendPathDelim(NewPath)+  'OutPrinter_'+FormatDateTime('yyyymmmddd-hhnnss',Now) + '.ps';
  result := fPrintFileName;
  //  TFilePrinterCanvas(Canvas).OutputFileName := FOutputFileName;
  end;

procedure TPostScriptClass.ViewFile;
 var
     AProcess:TProcess;
     EPath: String;
     FName: String;
 begin
    if fPrintFileName <> '' then
      FName := fPrintFileName
    else
      exit;
    ClosePrintFile;
    EPath := FindDefaultExecutablePath('kpdf');
    AProcess := TProcess.create(nil);
    AProcess.CommandLine := EPath +' '+fPrintFileName;
    AProcess.Options := AProcess.Options + [poWaitOnExit];
    AProcess.Execute;
    AProcess.CommandLine := 'rm '+fPrintFileName;
    AProcess.Execute;
    AProcess.Free;
 end;


end.
{

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Process;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
    AProcess:TProcess;
    EPath: String;
begin
   EPath := FindDefaultExecutablePath('okular');
   AProcess := TProcess.create(nil);
   AProcess.CommandLine := EPath +' /home/don/tmp/tst.ps';
   AProcess.Options := AProcess.Options + [poWaitOnExit];
   AProcess.Execute;
   AProcess.Free;
end;

end.

{
 TO DO
********************************************** 
//setLayout
{ % sets proper page size
  % repositions origin to "top left"

  % in: pageSizeX pageSizeY
  % pageSizeX and pageSizeY are numbers in points
  % example: 612 792 or 792 612

  /Y exch def
  /X exch def

  <</PageSize [X Y]>> setpagedevice  % Page Size (orientation implicit)
  0 Y translate   % moves origin to upper-left corner

  % need to redefine moveto to invert Y value
  /mt /moveto load def  % store the "real" moveto
  /moveto
  { % now write our own
    % on stack Y, X
    neg mt  % negate the Y, then call real moveto
  } bind def

} bind def
}
