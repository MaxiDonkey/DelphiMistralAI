unit MistralAI.Batch;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiMistralAI
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, REST.JsonReflect, System.JSON, REST.Json.Types,
  System.NetEncoding, System.Net.Mime, MistralAI.API.Params, MistralAI.API,
  MistralAI.Async.Support, MistralAI.Types;

type
  /// <summary>
  /// Represents a class for constructing and managing query parameters to list batch jobs in the MistralAI API.
  /// </summary>
  /// <remarks>
  /// This class provides methods to define filters such as page size, model name, creation date, and batch status,
  /// allowing precise control over the listing of batch jobs.
  /// </remarks>
  TBatchJobListParams = class(TUrlParam)
  public
    /// <summary>
    /// Sets the page number for pagination in the batch job list query.
    /// </summary>
    /// <param name="Value">
    /// The page number to retrieve (e.g., 1 for the first page).
    /// </param>
    /// <returns>
    /// Returns the instance of <c>TBatchJobListParams</c> for method chaining.
    /// </returns>
    function Page(const Value: Integer): TBatchJobListParams;
    /// <summary>
    /// Sets the number of items per page for pagination in the batch job list query.
    /// </summary>
    /// <param name="Value">
    /// The number of items to retrieve per page.
    /// </param>
    /// <returns>
    /// Returns the instance of <c>TBatchJobListParams</c> for method chaining.
    /// </returns>
    function PageSize(const Value: Integer): TBatchJobListParams;
    /// <summary>
    /// Filters the batch job list by the name of the model used for processing.
    /// </summary>
    /// <param name="Value">
    /// The model name to filter batch jobs (e.g., "mistral-small-latest").
    /// </param>
    /// <returns>
    /// Returns the instance of <c>TBatchJobListParams</c> for method chaining.
    /// </returns>
    function Model(const Value: string): TBatchJobListParams;
    /// <summary>
    /// Filters the batch job list by custom metadata associated with the batch.
    /// </summary>
    /// <param name="Value">
    /// The metadata key or value to filter batch jobs.
    /// </param>
    /// <returns>
    /// Returns the instance of <c>TBatchJobListParams</c> for method chaining.
    /// </returns>
    function Metadata(const Value: string): TBatchJobListParams;
    /// <summary>
    /// Filters the batch job list to include only jobs created after the specified timestamp.
    /// </summary>
    /// <param name="Value">
    /// A string representing the timestamp (e.g., ISO 8601 format).
    /// </param>
    /// <returns>
    /// Returns the instance of <c>TBatchJobListParams</c> for method chaining.
    /// </returns>
    function CreatedAfter(const Value: string): TBatchJobListParams;
    /// <summary>
    /// Filters the batch job list to include only jobs created by the current user.
    /// </summary>
    /// <param name="Value">
    /// A boolean value indicating whether to filter jobs by the current user.
    /// </param>
    /// <returns>
    /// Returns the instance of <c>TBatchJobListParams</c> for method chaining.
    /// </returns>
    function CreatedByMe(const Value: Boolean): TBatchJobListParams;
    /// <summary>
    /// Filters the batch job list by the current status of the jobs.
    /// </summary>
    /// <param name="Value">
    /// The status of the batch jobs to filter (e.g., <c>QUEUED</c>, <c>RUNNING</c>, <c>SUCCESS</c>, etc.).
    /// </param>
    /// <returns>
    /// Returns the instance of <c>TBatchJobListParams</c> for method chaining.
    /// </returns>
    function Status(const Value: TBatchStatus): TBatchJobListParams;
  end;

  /// <summary>
  /// Represents a class for managing parameters required to create a batch job for the MistralAI API.
  /// This class, <c>TBatchJobParams</c>, extends <c>TJSONParam</c> to construct and handle JSON payloads for batch job creation.
  ///</summary>
  ///<remarks>
  /// A batch job consists of multiple API requests, each identified by a unique <c>custom_id</c>.
  /// This class simplifies the process of defining the parameters and generating the required JSON payloads for a batch job.
  ///</remarks>
  TBatchJobParams = class(TJSONParam)
  public
    /// <summary>
    /// A list of the batch input file IDs.
    /// </summary>
    function InputFiles(const Value: TArray<string>): TBatchJobParams;
    /// <summary>
    /// Currently support <c>/v1/embeddings</c>, <c>/v1/chat/completions</c>, <c>/v1/fim/completions</c>, <c>/v1/moderations</c>, <c>/v1/chat/moderations</c>.
    /// </summary>
    function Endpoint(const Value: TEndPointType): TBatchJobParams;
    /// <summary>
    /// Model used for batch job process.
    /// </summary>
    function Model(const Value: string): TBatchJobParams;
    /// <summary>
    /// Optional custom metadata for the batch.
    /// </summary>
    /// <remarks>
    /// property name*: string [ 1 ... 512 ] characters
    /// <para>
    /// additional property
    /// </para>
    /// </remarks>
    function Metadata(const Value: TJSONObject): TBatchJobParams; overload;
    /// <summary>
    /// Optional custom metadata for the batch.
    /// </summary>
    /// <remarks>
    /// property name*: string [ 1 ... 512 ] characters
    /// <para>
    /// additional property
    /// </para>
    /// </remarks>
    function Metadata(const Value: TJSONParam): TBatchJobParams; overload;
    /// <summary>
    /// Batch generation can take up to 24 hours, although it might finish earlier.
    /// </summary>
    /// <remarks>
    /// <para>
    /// - Please note that processing speeds may be adjusted based on current demand and the volume of
    /// your request. Your batch results will only be accessible once the entire batch processing is
    /// complete.
    /// </para>
    /// <para>
    /// - Users can set timeout_hours when creating a job, which specifies the number of hours after
    /// which the job should expire. It defaults to 24 hours and cannot exceed this limit at present.
    /// This means that batches will expire if processing does not complete within 24 hours. This is
    /// subject to change in the future.
    /// </para>
    /// </remarks>
    function TimeoutHours(const Value: Integer): TBatchJobParams;
  end;

  /// <summary>
  /// Represents a class for capturing error information related to batch job processing in the MistralAI API.
  /// </summary>
  /// <remarks>
  /// This class provides details about errors encountered during batch job processing, including error messages and counts.
  /// </remarks>
  TBatchJobListDataError = class
  private
    FMessage: string;
    FCount: Int64;
  public
    /// <summary>
    /// Gets or sets the error message describing the issue encountered during batch job processing.
    /// </summary>
    /// <value>
    /// A string representing the error message (e.g., "Invalid input file format").
    /// </value>
    property &Message: string read FMessage write FMessage;
    /// <summary>
    /// Gets or sets the count of occurrences for this specific error.
    /// </summary>
    /// <value>
    /// An integer representing the number of times this error was encountered.
    /// </value>
    property Count: Int64 read FCount write FCount;
  end;

  /// <summary>
  /// Represents a batch job in the MistralAI API.
  /// </summary>
  /// <remarks>
  /// A batch job contains details about the processing status, input files, model used, metadata, and
  /// the results of the batch operation.
  /// </remarks>
  TBatchJob = class
  private
    FId: string;
    FObjet: string;
    [JsonNameAttribute('input_files')]
    FInputFiles: TArray<string>;
    [JsonReflectAttribute(ctString, rtString, TMetadataInterceptor)]
    FMetadata: string;
    [JsonReflectAttribute(ctString, rtString, TEndPointTypeInterceptor)]
    FEndpoint: TEndPointType;
    FModel: string;
    [JsonNameAttribute('output_file')]
    FOutputFile: string;
    [JsonNameAttribute('error_file')]
    FErrorFile: string;
    FErrors: TArray<TBatchJobListDataError>;
    [JsonReflectAttribute(ctString, rtString, TBatchStatusInterceptor)]
    FStatus: TBatchStatus;
    [JsonNameAttribute('created_at')]
    FCreatedAt: Int64;
    [JsonNameAttribute('total_requests')]
    FTotalRequests: Int64;
    [JsonNameAttribute('completed_requests')]
    FCompletedRequests: Int64;
    [JsonNameAttribute('succeeded_requests')]
    FSucceededRequests: Int64;
    [JsonNameAttribute('failed_requests')]
    FFailedRequests: Int64;
    [JsonNameAttribute('started_at')]
    FStartedAt: Int64;
    [JsonNameAttribute('completed_at')]
    FCompletedAt: Int64;
  public
    /// <summary>
    /// The unique identifier for the batch job.
    /// </summary>
    property Id: string read FId write FId;
    /// <summary>
    /// The name or description of the batch job.
    /// </summary>
    property &Objet: string read FObjet write FObjet;
    /// <summary>
    /// An array of file IDs used as input for the batch job.
    /// </summary>
    property InputFiles: TArray<string> read FInputFiles write FInputFiles;
    /// <summary>
    /// Custom metadata associated with the batch job.
    /// </summary>
    property Metadata: string read FMetadata write FMetadata;
    /// <summary>
    /// The endpoint used for the batch job (e.g., <c>/v1/chat/completions</c>).
    /// </summary>
    property Endpoint: TEndPointType read FEndpoint write FEndpoint;
    /// <summary>
    /// The model used for processing the batch job.
    /// </summary>
    property Model: string read FModel write FModel;
    /// <summary>
    /// The file ID of the output file containing the results of the batch job.
    /// </summary>
    property OutputFile: string read FOutputFile write FOutputFile;
    /// <summary>
    /// The file ID of the error file containing details of any errors during processing.
    /// </summary>
    property ErrorFile: string read FErrorFile write FErrorFile;
    /// <summary>
    /// An array of error details, including messages and counts, for the batch job.
    /// </summary>
    property Errors: TArray<TBatchJobListDataError> read FErrors write FErrors;
    /// <summary>
    /// The current status of the batch job (e.g., <c>QUEUED</c>, <c>RUNNING</c>, <c>SUCCESS</c>, <c>FAILED</c>).
    /// </summary>
    property Status: TBatchStatus read FStatus write FStatus;
    /// <summary>
    /// The timestamp (in Unix time) when the batch job was created.
    /// </summary>
    property CreatedAt: Int64 read FCreatedAt write FCreatedAt;
    /// <summary>
    /// The total number of requests in the batch job.
    /// </summary>
    property TotalRequests: Int64 read FTotalRequests write FTotalRequests;
     /// <summary>
    /// The number of requests that have been completed so far.
    /// </summary>
    property CompletedRequests: Int64 read FCompletedRequests write FCompletedRequests;
    /// <summary>
    /// The number of requests that succeeded during batch processing.
    /// </summary>
    property SucceededRequests: Int64 read FSucceededRequests write FSucceededRequests;
    /// <summary>
    /// The number of requests that failed during batch processing.
    /// </summary>
    property FailedRequests: Int64 read FFailedRequests write FFailedRequests;
    /// <summary>
    /// The timestamp (in Unix time) when the batch job started processing.
    /// </summary>
    property StartedAt: Int64 read FStartedAt write FStartedAt;
    /// <summary>
    /// The timestamp (in Unix time) when the batch job completed processing.
    /// </summary>
    property CompletedAt: Int64 read FCompletedAt write FCompletedAt;
    /// <summary>
    /// Frees the memory allocated for the batch job, including associated error details.
    /// </summary>
    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents a collection of batch jobs in the MistralAI API.
  /// </summary>
  /// <remarks>
  /// This class contains a list of batch jobs along with metadata about the collection,
  /// such as the total number of jobs and the object type.
  /// </remarks>
  TBatchJobList = class
  private
    FData: TArray<TBatchJob>;
    FObject: string;
    FTotal: Int64;
  public
    /// <summary>
    /// An array of batch jobs included in the list.
    /// </summary>
    property Data: TArray<TBatchJob> read FData write FData;
    /// <summary>
    /// The object type for the batch job list (e.g., <c>"batch_job_list"</c>).
    /// </summary>
    property &Object: string read FObject write FObject;
    /// <summary>
    /// The total number of batch jobs available in the list.
    /// </summary>
    property Total: Int64 read FTotal write FTotal;
    /// <summary>
    /// Frees the memory allocated for the batch job list, including associated batch jobs.
    /// </summary>
    destructor Destroy; override;
  end;

  /// <summary>
  /// Asynchronous callback parameters for operations returning a single <c>TBatchJob</c>.
  /// </summary>
  /// <remarks>
  /// Used when performing asynchronous operations that return a <c>TBatchJob</c> instance.
  /// </remarks>
  TAsynBatchJob = TAsyncCallBack<TBatchJob>;

  /// <summary>
  /// Asynchronous callback parameters for operations returning a single <c>TBatchJobList</c>.
  /// </summary>
  /// <remarks>
  /// Used when performing asynchronous operations that return a <c>TBatchJobList</c> instance.
  /// </remarks>
  TAsynBatchJobList = TAsyncCallBack<TBatchJobList>;

  /// <summary>
  /// Provides methods for interacting with the batch job routes in the MistralAI API.
  /// </summary>
  /// <remarks>
  /// This class enables the creation, retrieval, listing, and cancellation of batch jobs.
  /// It also supports both synchronous and asynchronous operations.
  /// </remarks>
  TBatchRoute = class(TMistralAIAPIRoute)
    /// <summary>
    /// Asynchronously creates a new batch job.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to configure the parameters for the batch job using a <c>TBatchJobParams</c> instance.
    /// </param>
    /// <param name="CallBacks">
    /// A function to handle the asynchronous callback, returning a <c>TAsynBatchJob</c>.
    /// </param>
    /// <remarks>
    /// <code>
    /// // WARNING - Move the following line into the main OnCreate
    /// // var MistralAI := TMistralAIFactory.CreateInstance(BaererKey);
    ///
    /// MistralAI.AsyncCreateJob(
    ///   procedure (Params: TBatchJobParams)
    ///   begin
    ///     // Define parameters for the batch job
    ///   end,
    ///
    ///   function: TAsynBatchJob
    ///   begin
    ///     Result.Sender := my_display_component;
    ///
    ///     Result.OnStart :=
    ///       procedure (Sender: TObject)
    ///       begin
    ///         // Handle the start
    ///       end;
    ///
    ///     Result.OnSuccess :=
    ///       procedure (Sender: TObject; Value: TBatchJob)
    ///       begin
    ///         // Handle the success result
    ///       end;
    ///
    ///     Result.OnError :=
    ///       procedure (Sender: TObject; Error: string)
    ///       begin
    ///         // Handle errors
    ///       end;
    ///   end);
    /// </code>
    /// </remarks>
    procedure AsyncCreateJob(ParamProc: TProc<TBatchJobParams>; CallBacks: TFunc<TAsynBatchJob>);
     /// <summary>
    /// Asynchronously retrieves a list of batch jobs.
    /// </summary>
    /// <param name="CallBacks">
    /// A function to handle the asynchronous callback, returning a <c>TAsynBatchJobList</c>.
    /// </param>
    /// <remarks>
    /// <code>
    /// // WARNING - Move the following line into the main OnCreate
    /// // var MistralAI := TMistralAIFactory.CreateInstance(BaererKey);
    ///
    /// MistralAI.AsyncList(
    ///   function: TAsynBatchJobList
    ///   begin
    ///     Result.Sender := my_display_component;
    ///
    ///     Result.OnStart :=
    ///       procedure (Sender: TObject)
    ///       begin
    ///         // Handle the start
    ///       end;
    ///
    ///     Result.OnSuccess :=
    ///       procedure (Sender: TObject; Value: TBatchJobList)
    ///       begin
    ///         // Handle the successful retrieval
    ///       end;
    ///
    ///     Result.OnError :=
    ///       procedure (Sender: TObject; Error: string)
    ///       begin
    ///         // Handle errors
    ///       end;
    ///   end);
    /// </code>
    /// </remarks>
    procedure AsyncList(CallBacks: TFunc<TAsynBatchJobList>); overload;
    /// <summary>
    /// Asynchronously retrieves a filtered list of batch jobs.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to configure the filter parameters using a <c>TBatchJobListParams</c> instance.
    /// </param>
    /// <param name="CallBacks">
    /// A function to handle the asynchronous callback, returning a <c>TAsynBatchJobList</c>.
    /// </param>
    /// <remarks>
    /// <code>
    /// // WARNING - Move the following line into the main OnCreate
    /// // var MistralAI := TMistralAIFactory.CreateInstance(BaererKey);
    ///
    /// MistralAI.AsyncList(
    ///   procedure (Params: TBatchJobListParams)
    ///   begin
    ///     // Define parameters for filtering batch jobs
    ///   end,
    ///
    ///   function: TAsynBatchJobList
    ///   begin
    ///     Result.Sender := my_display_component;
    ///
    ///     Result.OnStart :=
    ///       procedure (Sender: TObject)
    ///       begin
    ///         // Handle the start
    ///       end;
    ///
    ///     Result.OnSuccess :=
    ///       procedure (Sender: TObject; Value: TBatchJobList)
    ///       begin
    ///         // Handle the successful retrieval
    ///       end;
    ///
    ///     Result.OnError :=
    ///       procedure (Sender: TObject; Error: string)
    ///       begin
    ///         // Handle errors
    ///       end;
    ///   end);
    /// </code>
    /// </remarks>
    procedure AsyncList(ParamProc: TProc<TBatchJobListParams>; CallBacks: TFunc<TAsynBatchJobList>); overload;
    /// <summary>
    /// Asynchronously retrieves the details of a specific batch job.
    /// </summary>
    /// <param name="BatchId">
    /// The unique identifier of the batch job to retrieve.
    /// </param>
    /// <param name="CallBacks">
    /// A function to handle the asynchronous callback, returning a <c>TAsynBatchJob</c>.
    /// </param>
    /// <remarks>
    /// <code>
    /// // WARNING - Move the following line into the main OnCreate
    /// // var MistralAI := TMistralAIFactory.CreateInstance(BaererKey);
    ///
    /// MistralAI.AsyncRetrieve('batch-id-123',
    ///   function: TAsynBatchJob
    ///   begin
    ///     Result.Sender := my_display_component;
    ///
    ///     Result.OnStart :=
    ///       procedure (Sender: TObject)
    ///       begin
    ///         // Handle the start
    ///       end;
    ///
    ///     Result.OnSuccess :=
    ///       procedure (Sender: TObject; Value: TBatchJob)
    ///       begin
    ///         // Handle the successful retrieval of the batch job
    ///       end;
    ///
    ///     Result.OnError :=
    ///       procedure (Sender: TObject; Error: string)
    ///       begin
    ///         // Handle errors
    ///       end;
    ///   end);
    /// </code>
    /// </remarks>
    procedure AsyncRetrieve(const BatchId: string; CallBacks: TFunc<TAsynBatchJob>);
    /// <summary>
    /// Asynchronously cancels a specific batch job.
    /// </summary>
    /// <param name="BatchId">
    /// The unique identifier of the batch job to cancel.
    /// </param>
    /// <param name="CallBacks">
    /// A function to handle the asynchronous callback, returning a <c>TAsynBatchJob</c>.
    /// </param>
    /// <remarks>
    /// <code>
    /// // WARNING - Move the following line into the main OnCreate
    /// // var MistralAI := TMistralAIFactory.CreateInstance(BaererKey);
    ///
    /// MistralAI.AsyncCancel('batch-id-456',
    ///   function: TAsynBatchJob
    ///   begin
    ///     Result.Sender := my_display_component;
    ///
    ///     Result.OnStart :=
    ///       procedure (Sender: TObject)
    ///       begin
    ///         // Handle the start
    ///       end;
    ///
    ///     Result.OnSuccess :=
    ///       procedure (Sender: TObject; Value: TBatchJob)
    ///       begin
    ///         // Handle the successful cancellation of the batch job
    ///       end;
    ///
    ///     Result.OnError :=
    ///       procedure (Sender: TObject; Error: string)
    ///       begin
    ///         // Handle errors
    ///       end;
    ///   end);
    /// </code>
    /// </remarks>
    procedure AsyncCancel(const BatchId: string; CallBacks: TFunc<TAsynBatchJob>);
    /// <summary>
    /// Creates a new batch job.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to configure the parameters for the batch job using a <c>TBatchJobParams</c> instance.
    /// </param>
    /// <returns>
    /// Returns the created <c>TBatchJob</c> instance.
    /// </returns>
    /// <remarks>
    /// <code>
    /// // WARNING - Move the following line into the main OnCreate
    /// // var MistralAI := TMistralAIFactory.CreateInstance(BaererKey);
    ///
    /// var Value := MistralAI.Batch.CreateJob(
    ///   procedure (Params: TBatchJobParams)
    ///   begin
    ///     // Define parameters for the batch job
    ///   end);
    /// try
    ///   // Do something with the created batch job
    /// finally
    ///   Value.Free;
    /// end;
    /// </code>
    /// </remarks>
    function CreateJob(ParamProc: TProc<TBatchJobParams>): TBatchJob;
    /// <summary>
    /// Retrieves a list of batch jobs.
    /// </summary>
    /// <returns>
    /// Returns a <c>TBatchJobList</c> containing all available batch jobs.
    /// </returns>
    /// <remarks>
    /// <code>
    /// // WARNING - Move the following line into the main OnCreate
    /// // var MistralAI := TMistralAIFactory.CreateInstance(BaererKey);
    ///
    /// var Value := MistralAI.Batch.List;
    /// try
    ///   // Do something
    /// finally
    ///   Value.Free;
    /// end;
    /// </code>
    /// </remarks>
    function List: TBatchJobList; overload;
    /// <summary>
    /// Retrieves a filtered list of batch jobs.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to configure the filter parameters using a <c>TBatchJobListParams</c> instance.
    /// </param>
    /// <returns>
    /// Returns a <c>TBatchJobList</c> containing the filtered batch jobs.
    /// </returns>
    /// <remarks>
    /// <code>
    /// // WARNING - Move the following line into the main OnCreate
    /// // var MistralAI := TMistralAIFactory.CreateInstance(BaererKey);
    ///
    /// var Value := MistralAI.Batch.List(
    ///   procedure (Params: TBatchJobListParams)
    ///   begin
    ///     // Define parameters for the batch job
    ///   end);
    /// try
    ///   // Do something with the created batch job
    /// finally
    ///   Value.Free;
    /// end;
    /// </code>
    /// </remarks>
    function List(ParamProc: TProc<TBatchJobListParams>): TBatchJobList; overload;
    /// <summary>
    /// Retrieves the details of a specific batch job.
    /// </summary>
    /// <param name="BatchId">
    /// The unique identifier of the batch job to retrieve.
    /// </param>
    /// <returns>
    /// Returns the <c>TBatchJob</c> instance corresponding to the given batch ID.
    /// </returns>
    /// <remarks>
     /// <code>
    /// // WARNING - Move the following line into the main OnCreate
    /// // var MistralAI := TMistralAIFactory.CreateInstance(BaererKey);
    ///
    /// var Job := MistralAI.Batch.Retrieve('batch-id-456');
    /// try
    ///   // Work with the retrieved batch job
    /// finally
    ///   Job.Free;
    /// end;
    /// </code>
    /// </remarks>
    function Retrieve(const BatchId: string): TBatchJob;
    /// <summary>
    /// Cancels a specific batch job.
    /// </summary>
    /// <param name="BatchId">
    /// The unique identifier of the batch job to cancel.
    /// </param>
    /// <returns>
    /// Returns the updated <c>TBatchJob</c> instance after cancellation.
    /// </returns>
    /// <remarks>
    /// <code>
    /// // WARNING - Move the following line into the main OnCreate
    /// // var MistralAI := TMistralAIFactory.CreateInstance(BaererKey);
    ///
    /// var CanceledJob := MistralAI.Batch.Cancel('batch-id-789');
    /// try
    ///   // Handle the cancellation result
    /// finally
    ///   CanceledJob.Free;
    /// end;
    /// </code>
    /// </remarks>
    function Cancel(const BatchId: string): TBatchJob;
  end;

implementation

{ TBatchJobListParams }

function TBatchJobListParams.CreatedAfter(
  const Value: string): TBatchJobListParams;
begin
  Result := TBatchJobListParams(Add('created_after', Value));
end;

function TBatchJobListParams.CreatedByMe(
  const Value: Boolean): TBatchJobListParams;
begin
  Result := TBatchJobListParams(Add('created_by_me', Value));
end;

function TBatchJobListParams.Metadata(const Value: string): TBatchJobListParams;
begin
  Result := TBatchJobListParams(Add('metadata', Value));
end;

function TBatchJobListParams.Model(const Value: string): TBatchJobListParams;
begin
  Result := TBatchJobListParams(Add('model', Value));
end;

function TBatchJobListParams.Page(const Value: Integer): TBatchJobListParams;
begin
  Result := TBatchJobListParams(Add('page', Value));
end;

function TBatchJobListParams.PageSize(
  const Value: Integer): TBatchJobListParams;
begin
  Result := TBatchJobListParams(Add('page_size', Value));
end;

function TBatchJobListParams.Status(
  const Value: TBatchStatus): TBatchJobListParams;
begin
  Result := TBatchJobListParams(Add('status', Value.ToString));
end;

{ TBatchJob }

destructor TBatchJob.Destroy;
begin
  for var Item in FErrors do
    Item.Free;
  inherited;
end;

{ TBatchJobList }

destructor TBatchJobList.Destroy;
begin
  for var Item in FData do
    Item.Free;
  inherited;
end;

{ TBatchRoute }

procedure TBatchRoute.AsyncCancel(const BatchId: string;
  CallBacks: TFunc<TAsynBatchJob>);
begin
  with TAsyncCallBackExec<TAsynBatchJob, TBatchJob>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TBatchJob
      begin
        Result := Self.Cancel(BatchId);
      end);
  finally
    Free;
  end;
end;

procedure TBatchRoute.AsyncCreateJob(ParamProc: TProc<TBatchJobParams>;
  CallBacks: TFunc<TAsynBatchJob>);
begin
  with TAsyncCallBackExec<TAsynBatchJob, TBatchJob>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TBatchJob
      begin
        Result := Self.CreateJob(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TBatchRoute.AsyncList(ParamProc: TProc<TBatchJobListParams>;
  CallBacks: TFunc<TAsynBatchJobList>);
begin
  with TAsyncCallBackExec<TAsynBatchJobList, TBatchJobList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TBatchJobList
      begin
        Result := Self.List(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TBatchRoute.AsyncList(CallBacks: TFunc<TAsynBatchJobList>);
begin
  with TAsyncCallBackExec<TAsynBatchJobList, TBatchJobList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TBatchJobList
      begin
        Result := Self.List;
      end);
  finally
    Free;
  end;
end;

procedure TBatchRoute.AsyncRetrieve(const BatchId: string;
  CallBacks: TFunc<TAsynBatchJob>);
begin
  with TAsyncCallBackExec<TAsynBatchJob, TBatchJob>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TBatchJob
      begin
        Result := Self.Retrieve(BatchId);
      end);
  finally
    Free;
  end;
end;

function TBatchRoute.Cancel(const BatchId: string): TBatchJob;
begin
  Result := API.Post<TBatchJob>(Format('batch/jobs/%s/cancel', [BatchId]));
end;

function TBatchRoute.CreateJob(ParamProc: TProc<TBatchJobParams>): TBatchJob;
begin
  Result := API.Post<TBatchJob, TBatchJobParams>('batch/jobs', ParamProc);
end;

function TBatchRoute.List: TBatchJobList;
begin
  Result := API.Get<TBatchJobList>('batch/jobs');
end;

function TBatchRoute.List(ParamProc: TProc<TBatchJobListParams>): TBatchJobList;
begin
  Result := API.Get<TBatchJobList, TBatchJobListParams>('batch/jobs', ParamProc);
end;

function TBatchRoute.Retrieve(const BatchId: string): TBatchJob;
begin
  Result := API.Get<TBatchJob>(Format('batch/jobs/%s', [BatchId]));
end;

{ TBatchJobParams }

function TBatchJobParams.Endpoint(const Value: TEndPointType): TBatchJobParams;
begin
  Result := TBatchJobParams(Add('endpoint', Value.ToString));
end;

function TBatchJobParams.InputFiles(
  const Value: TArray<string>): TBatchJobParams;
begin
  Result := TBatchJobParams(Add('input_files', Value));
end;

function TBatchJobParams.Metadata(const Value: TJSONObject): TBatchJobParams;
begin
  Result := TBatchJobParams(Add('metadata', Value));
end;

function TBatchJobParams.Metadata(const Value: TJSONParam): TBatchJobParams;
begin
  Result := TBatchJobParams(Add('metadata', Value.Detach));
end;

function TBatchJobParams.Model(const Value: string): TBatchJobParams;
begin
  Result := TBatchJobParams(Add('model', Value));
end;

function TBatchJobParams.TimeoutHours(const Value: Integer): TBatchJobParams;
begin
  Result := TBatchJobParams(Add('timeout_hours', Value));
end;

end.
