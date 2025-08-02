unit MistralAI.OCR;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiMistralAI
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, System.Threading,
  REST.Json.Types, REST.JsonReflect, System.JSON,
  MistralAI.API.Params, MistralAI.API, MistralAI.Functions.Core, MistralAI.Functions.Tools,
  MistralAI.Async.Support, MistralAI.Async.Params, MistralAI.Types, MistralAI.Async.Promise,
  MistralAI.Schema;

type
  TOcrImageUrl = class(TJSONParam)
    /// <summary>
    /// string (Url)
    /// </summary>
    function Url(const Value: string): TOcrImageUrl;

    /// <summary>
    /// Detail (string) or Detail (null) (Detail)
    /// </summary>
    function Detail(const Value: string): TOcrImageUrl;
  end;

  TOcrDocumentParams = class(TJSONParam)
    /// <summary>
    /// Enum: "document_url" or "image_url"
    /// </summary>
    function &Type(const Value: string): TOcrDocumentParams;

    /// <summary>
    /// string (Document Url)
    /// </summary>
    function DocumentUrl(const Value: string): TOcrDocumentParams;

    /// <summary>
    /// The filename of the document
    /// </summary>
    function DocumentName(const Value: string): TOcrDocumentParams;

    /// <summary>
    /// ImageURL (object) or Image Url (string) (Image Url)
    /// </summary>
    function ImageUrl(const Value: TOcrImageUrl): TOcrDocumentParams;

    class function NewDocument: TOcrDocumentParams;
    class function NewImage: TOcrDocumentParams;
  end;

  TAnnotationFormat = class(TJSONParam)
    /// <summary>
    /// An object specifying the format that the model must output. Setting to { "type": "json_object" }
    /// enables JSON mode, which guarantees the message the model generates is in JSON. When using JSON
    /// mode you MUST also instruct the model to produce JSON yourself with a system or a user message.
    /// </summary>
    /// <param name="Value">
    /// Enum: "text" "json_object" "json_schema"
    /// </param>
    function &Type(const Value: string): TAnnotationFormat;

    /// <summary>
    /// JsonSchema (object)
    /// </summary>
    function JsonSchema(const Value: TResponseSchemaParams): TAnnotationFormat;
  end;

  TOCRFormat = record
  public
    /// <summary>
    /// Creates an annotation format configured for plain text output.
    /// </summary>
    /// <returns>
    /// A <c>TAnnotationFormat</c> instance with its type set to <c>"text"</c>, instructing the OCR to return unstructured text.
    /// </returns>
    /// <remarks>
    /// Use this format when you require the OCR result as simple text rather than JSON or schema‑based annotations.
    /// </remarks>
    class function Text: TAnnotationFormat; static;

    /// <summary>
    /// Creates an annotation format configured for JSON object output.
    /// </summary>
    /// <returns>
    /// A <c>TAnnotationFormat</c> instance with its type set to <c>"json_object"</c>, instructing the OCR to return the result as a structured JSON object.
    /// </returns>
    /// <remarks>
    /// Use this format when you need the OCR result in JSON mode, ensuring the response is valid JSON that can be parsed directly into data structures.
    /// Remember to include explicit instructions to the model to produce JSON if using JSON mode.
    /// </remarks>
    class function Json_object: TAnnotationFormat; static;

    /// <summary>
    /// Creates an annotation format configured for JSON schema output.
    /// </summary>
    /// <param name="Value">
    /// A <c>TResponseSchemaParams</c> instance defining the JSON schema that the OCR should use to structure its output.
    /// </param>
    /// <returns>
    /// A <c>TAnnotationFormat</c> instance with its type set to <c>"json_schema"</c> and the specified schema, instructing the OCR to return data annotated according to that schema.
    /// </returns>
    /// <remarks>
    /// Use this format when you need the OCR results to conform to a predefined JSON schema for structured data extraction.
    /// Make sure the provided <c>Value</c> accurately represents the schema you expect in the response.
    /// </remarks>
    class function Json_schema(const Value: TResponseSchemaParams): TAnnotationFormat; static;
  end;

  TOcrParams = class(TJSONParam)
    /// <summary>
    /// OCR model eg mistral-ocr-latest.
    /// </summary>
    function Model(const Value: string): TOCRParams;

    /// <summary>
    /// string(Id)
    /// </summary>
    function Id(const Value: string): TOCRParams;

    /// <summary>
    /// Document to run OCR on
    /// </summary>
    function Document(const Value: TOcrDocumentParams): TOCRParams;

    /// <summary>
    /// Specific pages user wants to process in various formats: single number, range, or list of both.
    /// Starts from 0
    /// </summary>
    function Pages(const Value: TArray<Integer>): TOCRParams;

    /// <summary>
    /// Include image URLs in response
    /// </summary>
    function IncludeImageBase64(const Value: Boolean = True): TOCRParams;

    /// <summary>
    /// Max images to extract
    /// </summary>
    function ImageLimit(const Value: Integer): TOCRParams;

    /// <summary>
    /// Minimum height and width of image to extract
    /// </summary>
    function ImageMinSize(const Value: Integer): TOCRParams;

    /// <summary>
    /// Structured output class for extracting useful information from each extracted bounding box / image
    /// from document. Only json_schema is valid for this field
    /// </summary>
    function BboxAnnotationFormat(const Value: TAnnotationFormat): TOCRParams; overload;

    /// <summary>
    /// Structured output class for extracting useful information from each extracted bounding box / image
    /// from document. Only json_schema is valid for this field
    /// </summary>
    function BboxAnnotationFormat(const Value: TJSONObject): TOCRParams; overload;

    /// <summary>
    /// Structured output class for extracting useful information from the entire document. Only json_schema
    /// is valid for this field
    /// </summary>
    function DocumentAnnotationFormat(const Value: TAnnotationFormat): TOCRParams; overload;

    /// <summary>
    /// Structured output class for extracting useful information from the entire document. Only json_schema
    /// is valid for this field
    /// </summary>
    function DocumentAnnotationFormat(const Value: TJSONObject): TOCRParams; overload;
  end;

  TOcrPageImageCore = class
  private
    FId: string;
    [JsonNameAttribute('top_left_x')]
    FTopLeftX: Integer;
    [JsonNameAttribute('top_left_y')]
    FTopLeftY: Integer;
    [JsonNameAttribute('bottom_right_x')]
    FBottomRightX: Integer;
    [JsonNameAttribute('bottom_right_y')]
    FBottomRightY: Integer;
    [JsonNameAttribute('image_base64')]
    FImageBase64: string;
    [JsonNameAttribute('image_annotation')]
    FImageAnnotation: string;
  public
    /// <summary>
    /// Image ID for extracted image in a page
    /// </summary>
    property Id: string read FId write FId;

    /// <summary>
    /// X coordinate of top-left corner of the extracted image
    /// </summary>
    property TopLeftX: Integer read FTopLeftX write FTopLeftX;

    /// <summary>
    /// Y coordinate of top-left corner of the extracted image
    /// </summary>
    property TopLeftY: Integer read FTopLeftY write FTopLeftY;

    /// <summary>
    /// X coordinate of bottom-right corner of the extracted image
    /// </summary>
    property BottomRightX: Integer read FBottomRightX write FBottomRightX;

    /// <summary>
    /// Y coordinate of bottom-right corner of the extracted image
    /// </summary>
    property BottomRightY: Integer read FBottomRightY write FBottomRightY;

    /// <summary>
    /// Base64 string of the extracted image
    /// </summary>
    property ImageBase64: string read FImageBase64 write FImageBase64;

    /// <summary>
    /// Annotation of the extracted image in json str
    /// </summary>
    property ImageAnnotation: string read FImageAnnotation write FImageAnnotation;
  end;

  TOcrPageImage = class(TOcrPageImageCore)
  private
    FFileName: string;
  public
    /// <summary>
    /// Retrieves the generated image as a <c>TStream</c>.
    /// </summary>
    /// <returns>
    /// A <c>TStream</c> containing the decoded image data.
    /// </returns>
    /// <remarks>
    /// This method decodes the base64-encoded image data and returns it as a stream.
    /// The caller is responsible for freeing the returned stream.
    /// </remarks>
    /// <exception cref="Exception">
    /// Raises an exception if both the image data are empty.
    /// </exception>
    function GetStream: TStream;

    /// <summary>
    /// Saves the generated image to a file.
    /// </summary>
    /// <param name="FileName">
    /// The file path where the image will be saved.
    /// </param>
    /// <remarks>
    /// This method decodes the base64-encoded image data and saves it to the specified file.
    /// </remarks>
    /// <exception cref="Exception">
    /// Raises an exception if the image data cannot be decoded or saved.
    /// </exception>
    procedure SaveToFile(const FileName: string);

    /// <summary>
    /// Gets the file name where the image was saved.
    /// </summary>
    /// <value>
    /// The file path as a string.
    /// </value>
    /// <remarks>
    /// This property holds the file name specified in the last call to <c>SaveToFile</c>.
    /// </remarks>
    property FileName: string read FFileName write FFileName;
  end;

  TOcrPageDimensions = class
  private
    FDpi: Integer;
    FHeight: Integer;
    FWidth: Integer;
  public
    /// <summary>
    /// Dots per inch of the page-image
    /// </summary>
    property Dpi: Integer read FDpi write FDpi;

    /// <summary>
    /// Height of the image in pixels
    /// </summary>
    property Height: Integer read FHeight write FHeight;

    /// <summary>
    /// Width of the image in pixels
    /// </summary>
    property Width: Integer read FWidth write FWidth;
  end;

  TOcrPage = class
  private
    FIndex: Integer;
    FMarkdown: string;
    FImages: TArray<TOcrPageImage>;
    FDimensions: TOcrPageDimensions;
  public
    /// <summary>
    /// The page index in a pdf document starting from 0
    /// </summary>
    property Index: Integer read FIndex write FIndex;

    /// <summary>
    /// The markdown string response of the page
    /// </summary>
    property Markdown: string read FMarkdown write FMarkdown;

    /// <summary>
    /// List of all extracted images in the page
    /// </summary>
    property Images: TArray<TOcrPageImage> read FImages write FImages;

    /// <summary>
    /// The dimensions of the PDF Page's screenshot image
    /// </summary>
    property Dimensions: TOcrPageDimensions read FDimensions write FDimensions;

    destructor Destroy; override;
  end;

  TUsageInfo = class
  private
    [JsonNameAttribute('pages_processed')]
    FPagesProcessed: Integer;
    [JsonNameAttribute('doc_size_bytes')]
    FDocSizeBytes: Integer;
  public
    /// <summary>
    /// Number of pages processed
    /// </summary>
    property PagesProcessed: Integer read FPagesProcessed write FPagesProcessed;

    /// <summary>
    /// Document size in bytes
    /// </summary>
    property DocSizeBytes: Integer read FDocSizeBytes write FDocSizeBytes;
  end;

  TOcr = class(TJSONFingerprint)
  private
    FPages: TArray<TOcrPage>;
    FModel: string;
    [JsonNameAttribute('document_annotation')]
    FDocumentAnnotation: string;
    [JsonNameAttribute('usage_info')]
    FUsageInfo: TUsageInfo;
  public
    /// <summary>
    /// List of OCR info for pages.
    /// </summary>
    property Pages: TArray<TOcrPage> read FPages write FPages;

    /// <summary>
    /// The model used to generate the OCR.
    /// </summary>
    property Model: string read FModel write FModel;

    /// <summary>
    /// Formatted response in the request_format if provided in json str
    /// </summary>
    property DocumentAnnotation: string read FDocumentAnnotation write FDocumentAnnotation;

    /// <summary>
    /// Usage info for the OCR request.
    /// </summary>
    property UsageInfo: TUsageInfo read FUsageInfo write FUsageInfo;

    destructor Destroy; override;
  end;

  /// <summary>
  /// Manages asynchronous OCR callbacks for a document processing request using <c>TOcr</c> as the response type.
  /// </summary>
  /// <remarks>
  /// The <c>TAsyncOcr</c> type is an alias for <c>TAsyncCallback&lt;TOcr&gt;</c>, providing event handlers to track
  /// the lifecycle of an OCR operation. Assign handlers such as OnStart, OnSuccess, and OnError to respond to
  /// the various stages of the asynchronous OCR process, enabling non‐blocking, event‐driven document recognition.
  /// </remarks>
  TAsyncOcr = TAsyncCallback<TOcr>;

  /// <summary>
  /// Provides a promise‐based interface for an asynchronous OCR operation, returning a <c>TOcr</c> result when complete.
  /// </summary>
  /// <remarks>
  /// <c>TPromiseOcr</c> is an alias for <c>TPromiseCallback&lt;TOcr&gt;</c>. Use this type to initiate an OCR request
  /// and await its completion in a promise style, simplifying asynchronous flows by resolving with the OCR response
  /// or rejecting with an error if the operation fails.
  /// </remarks>
  TPromiseOcr = TPromiseCallback<TOcr>;

  /// <summary>
  /// Provides client routing for OCR operations against the Document AI API.
  /// </summary>
  /// <remarks>
  /// <para><c>TOcrRoute</c> inherits from <c>TMistralAIAPIRoute</c> and exposes methods to perform OCR requests.</para>
  /// <para>The <c>Create</c> method performs a synchronous OCR request and returns a <c>TOcr</c> result.</para>
  /// <para>The <c>AsyncCreate</c> method executes an OCR request with event handlers for OnStart, OnSuccess, and OnError, enabling non‑blocking, callback‑driven workflows.</para>
  /// <para>The <c>AsyncAwaitCreate</c> method returns a <c>TPromise&lt;TOcr&gt;</c>, allowing promise‑based asynchronous OCR operations that resolve with a <c>TOcr</c> or reject on error.</para>
  /// </remarks>
  TOcrRoute = class(TMistralAIAPIRoute)
    /// <summary>
    /// Asynchronously submits an OCR request configured by <paramref name="ParamProc"/> and returns a promise for the result.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure in which you configure the <c>TOcrParams</c>—for example, selecting the OCR model, specifying the document or image source, pages to process, and annotation formats.
    /// </param>
    /// <param name="Callbacks">
    /// An optional function that returns a <c>TPromiseOcr</c> to attach OnSuccess and OnError handlers in promise style. If <c>nil</c>, no additional callbacks are registered.
    /// </param>
    /// <returns>
    /// A <c>TPromise&lt;TOcr&gt;</c> that resolves with the completed <c>TOcr</c> response when the OCR operation finishes, or rejects with an exception if an error occurs.
    /// </returns>
    /// <remarks>
    /// Use this method for promise‑based asynchronous workflows, allowing you to await the OCR result directly or chain further processing steps.
    /// </remarks>
    function AsyncAwaitCreate(const ParamProc: TProc<TOcrParams>;
      const Callbacks: TFunc<TPromiseOcr> = nil): TPromise<TOcr>;

    /// <summary>
    /// Sends a synchronous OCR request configured by <paramref name="ParamProc"/> and returns the OCR result.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure in which you configure the <c>TOcrParams</c>—for example, selecting the OCR model, specifying the document or image source, pages to process, and annotation formats.
    /// </param>
    /// <returns>
    /// A <c>TOcr</c> object containing the OCR results, including page markdown, extracted images, annotations, and usage information.
    /// </returns>
    /// <remarks>
    /// Use this method for blocking, synchronous workflows. The call will raise an exception if the request fails or if parameters are invalid. Ensure that you free the returned <c>TOcr</c> instance when it is no longer needed.
    /// </remarks>
    function Create(const ParamProc: TProc<TOcrParams>): TOcr;

    /// <summary>
    /// Executes an OCR request asynchronously using callback handlers.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure in which you configure the <c>TOcrParams</c>—for example, selecting the OCR model, specifying the document or image source, pages to process, and annotation formats.
    /// </param>
    /// <param name="Callbacks">
    /// A function that returns a <c>TAsyncOcr</c> record, allowing you to assign handlers such as OnStart, OnSuccess, and OnError for event‑driven, non‑blocking workflows.
    /// </param>
    /// <remarks>
    /// Use this method when you need fine‑grained control over the OCR process via callbacks, enabling your application to respond to each stage of the operation without blocking the main thread.
    /// </remarks>
    procedure AsyncCreate(const ParamProc: TProc<TOcrParams>;
      const Callbacks: TFunc<TAsyncOcr>);
  end;

implementation

uses
  MistralAI.NetEncoding.Base64;

{ TOCRParams }

function TOcrParams.BboxAnnotationFormat(
  const Value: TAnnotationFormat): TOCRParams;
begin
  Result := TOCRParams(Add('bbox_annotation_format', Value.Detach));
end;

function TOcrParams.BboxAnnotationFormat(
  const Value: TJSONObject): TOCRParams;
begin
  Result := TOCRParams(Add('bbox_annotation_format', Value));
end;

function TOcrParams.Document(const Value: TOcrDocumentParams): TOCRParams;
begin
  Result := TOCRParams(Add('document', Value.Detach));
end;

function TOcrParams.DocumentAnnotationFormat(
  const Value: TJSONObject): TOCRParams;
begin
  Result := TOCRParams(Add('document_annotation_format', Value));
end;

function TOcrParams.DocumentAnnotationFormat(
  const Value: TAnnotationFormat): TOCRParams;
begin
  Result := TOCRParams(Add('document_annotation_format', Value.Detach));
end;

function TOcrParams.Id(const Value: string): TOCRParams;
begin
  Result := TOCRParams(Add('id', Value));
end;

function TOcrParams.ImageLimit(const Value: Integer): TOCRParams;
begin
  Result := TOCRParams(Add('image_limit', Value));
end;

function TOcrParams.ImageMinSize(const Value: Integer): TOCRParams;
begin
  Result := TOCRParams(Add('image_min_size', Value));
end;

function TOcrParams.IncludeImageBase64(const Value: Boolean): TOCRParams;
begin
  Result := TOCRParams(Add('include_image_base64', Value));
end;

function TOcrParams.Model(const Value: string): TOCRParams;
begin
  Result := TOCRParams(Add('model', Value));
end;

function TOcrParams.Pages(const Value: TArray<Integer>): TOCRParams;
begin
  Result := TOCRParams(Add('pages', Value));
end;

{ TOcrDocumentParams }

function TOcrDocumentParams.&Type(const Value: string): TOcrDocumentParams;
begin
  Result := TOcrDocumentParams(Add('type', Value));
end;

function TOcrDocumentParams.DocumentName(
  const Value: string): TOcrDocumentParams;
begin
  Result := TOcrDocumentParams(Add('document_name', Value));
end;

function TOcrDocumentParams.DocumentUrl(
  const Value: string): TOcrDocumentParams;
begin
  Result := TOcrDocumentParams(Add('document_url', PdfUrlCheck(Value)));
end;

function TOcrDocumentParams.ImageUrl(
  const Value: TOcrImageUrl): TOcrDocumentParams;
begin
  Result := TOcrDocumentParams(Add('image_url', Value.Detach));
end;

class function TOcrDocumentParams.NewDocument: TOcrDocumentParams;
begin
  Result := TOcrDocumentParams.Create.&Type('document_url');
end;

class function TOcrDocumentParams.NewImage: TOcrDocumentParams;
begin
  Result := TOcrDocumentParams.Create.&Type('image_url');
end;

{ TOcrImageUrl }

function TOcrImageUrl.Detail(const Value: string): TOcrImageUrl;
begin
  Result := TOcrImageUrl(Add('detail', Value));
end;

function TOcrImageUrl.Url(const Value: string): TOcrImageUrl;
begin
  Result := TOcrImageUrl(Add('url', ImageUrlCheck(Value)));
end;

{ TAnnotationFormat }

function TAnnotationFormat.JsonSchema(
  const Value: TResponseSchemaParams): TAnnotationFormat;
begin
  Result := TAnnotationFormat(Add('json_schema', Value.Detach));
end;

function TAnnotationFormat.&Type(const Value: string): TAnnotationFormat;
begin
  Result := TAnnotationFormat(Add('type', Value));
end;

{ TOCRFormat }

class function TOCRFormat.Json_object: TAnnotationFormat;
begin
  Result := TAnnotationFormat.Create.&Type('json_object');
end;

class function TOCRFormat.Json_schema(
  const Value: TResponseSchemaParams): TAnnotationFormat;
begin
  Result := TAnnotationFormat.Create.&Type('json_schema').JsonSchema(Value);
end;

class function TOCRFormat.Text: TAnnotationFormat;
begin
  Result := TAnnotationFormat.Create.&Type('text');
end;

{ TOcr }

destructor TOcr.Destroy;
begin
  for var Item in FPages do
    Item.Free;
  if Assigned(FUsageInfo) then
    FUsageInfo.Free;
  inherited;
end;

{ TOcrPage }

destructor TOcrPage.Destroy;
begin
  for var Item in FImages do
    Item.Free;
  if Assigned(FDimensions) then
    FDimensions.Free;
  inherited;
end;

{ TOcrRoute }

function TOcrRoute.AsyncAwaitCreate(const ParamProc: TProc<TOcrParams>;
  const Callbacks: TFunc<TPromiseOcr>): TPromise<TOcr>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TOcr>(
    procedure(const CallbackParams: TFunc<TAsyncOcr>)
    begin
      AsyncCreate(ParamProc, CallbackParams);
    end,
    Callbacks);
end;

procedure TOcrRoute.AsyncCreate(const ParamProc: TProc<TOcrParams>;
  const Callbacks: TFunc<TAsyncOcr>);
begin
  with TAsyncCallBackExec<TAsyncOcr, TOcr>.Create(Callbacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TOcr
      begin
        Result := Self.Create(ParamProc);
      end);
  finally
    Free;
  end;
end;

function TOcrRoute.Create(const ParamProc: TProc<TOcrParams>): TOcr;
begin
  Result := API.Post<TOcr, TOcrParams>('ocr', ParamProc);
end;

{ TOcrPageImage }

function TOcrPageImage.GetStream: TStream;
begin
  {--- Create a memory stream to write the decoded content. }
  Result := TMemoryStream.Create;
  try
    {--- Convert the base-64 string directly into the memory stream. }
    DecodeBase64ToStream(ImageBase64, Result)
  except
    Result.Free;
    raise;
  end;
end;

procedure TOcrPageImage.SaveToFile(const FileName: string);
begin
  try
    Self.FFileName := FileName;
    {--- Perform the decoding operation and save it into the file specified by the FileName parameter. }
    DecodeBase64ToFile(ImageBase64, FileName)
  except
    raise;
  end;
end;

end.
