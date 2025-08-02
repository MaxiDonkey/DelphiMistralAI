unit MistralAI.Async.Parallel;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.SyncObjs, System.Threading,
  MistralAI.Types, MistralAI.Async.Params, MistralAI.Async.Support, MistralAI.API.Params,
  MistralAI.Async.Promise;

type
  /// <summary>
  /// Represents an item in a bundle of chat prompts and responses.
  /// </summary>
  /// <remarks>
  /// This class stores information about a single chat request, including its index,
  /// associated prompt, generated response, and related chat object.
  /// It is used within a <c>TBundleList</c> to manage multiple asynchronous chat requests.
  /// </remarks>
  TBundleItem = class
  private
    FIndex: Integer;
    FFinishIndex: Integer;
    FPrompt: string;
    FResponse: string;
    FChat: TObject;
  public
    /// <summary>
    /// Gets or sets the index of the item in the bundle.
    /// </summary>
    property Index: Integer read FIndex write FIndex;
    /// <summary>
    /// Gets or sets the finishing index of the item after processing.
    /// </summary>
    property FinishIndex: Integer read FFinishIndex write FFinishIndex;
    /// <summary>
    /// Gets or sets the prompt associated with this bundle item.
    /// </summary>
    property Prompt: string read FPrompt write FPrompt;
    /// <summary>
    /// Gets or sets the response generated for the given prompt.
    /// </summary>
    property Response: string read FResponse write FResponse;
    /// <summary>
    /// Gets or sets the chat object associated with this item.
    /// </summary>
    /// <remarks>
    /// This object contains additional information about the chat session,
    /// including metadata related to the AI-generated response.
    /// </remarks>
    property Chat: TObject read FChat write FChat;
    /// <summary>
    /// Destroys the <c>TBundleItem</c> instance and releases associated resources.
    /// </summary>
    /// <remarks>
    /// If a chat object (<c>FChat</c>) is assigned, it is freed upon destruction.
    /// </remarks>
    destructor Destroy; override;
  end;

  /// <summary>
  /// Manages a collection of <c>TBundleItem</c> objects.
  /// </summary>
  /// <remarks>
  /// This class provides methods to add, retrieve, and count items in a bundle.
  /// It is designed to store multiple chat request items processed in parallel.
  /// The internal storage uses a <c>TObjectList&lt;TBundleItem&gt;</c> with automatic memory management.
  /// </remarks>
  TBundleList = class
  private
    FItems: TObjectList<TBundleItem>;
  public
    /// <summary>
    /// Initializes a new instance of <c>TBundleList</c>.
    /// </summary>
    /// <remarks>
    /// This constructor creates an internal list of <c>TBundleItem</c> objects,
    /// ensuring that items are automatically freed when the list is destroyed.
    /// </remarks>
    constructor Create;
    /// <summary>
    /// Destroys the <c>TBundleList</c> instance and releases all associated resources.
    /// </summary>
    /// <remarks>
    /// This destructor frees all <c>TBundleItem</c> objects stored in the list.
    /// </remarks>
    destructor Destroy; override;
    /// <summary>
    /// Adds a new item to the bundle.
    /// </summary>
    /// <param name="AIndex">
    /// The index of the new item in the bundle.
    /// </param>
    /// <returns>
    /// The newly created <c>TBundleItem</c> instance.
    /// </returns>
    /// <remarks>
    /// This method creates a new <c>TBundleItem</c>, assigns it the specified index,
    /// and adds it to the internal list.
    /// </remarks>
    function Add(const AIndex: Integer): TBundleItem;
    /// <summary>
    /// Retrieves an item from the bundle by its index.
    /// </summary>
    /// <param name="AIndex">
    /// The zero-based index of the item to retrieve.
    /// </param>
    /// <returns>
    /// The <c>TBundleItem</c> instance at the specified index.
    /// </returns>
    /// <exception cref="Exception">
    /// Raised if the specified index is out of bounds.
    /// </exception>
    function Item(const AIndex: Integer): TBundleItem;
    /// <summary>
    /// Gets the total number of items in the bundle.
    /// </summary>
    /// <returns>
    /// The number of items stored in the bundle.
    /// </returns>
    function Count: Integer;
    /// <summary>
    /// Provides direct access to the internal list of <c>TBundleItem</c> objects.
    /// </summary>
    property Items: TObjectList<TBundleItem> read FItems write FItems;
  end;

  /// <summary>
  /// Represents an asynchronous callback buffer for handling chat responses.
  /// </summary>
  /// <remarks>
  /// This class is a specialized type used to manage asynchronous operations
  /// related to chat request processing. It inherits from <c>TAsynCallBack&lt;TBundleList&gt;</c>,
  /// enabling structured handling of callback events.
  /// </remarks>
  TAsynBundleList = TAsyncCallBack<TBundleList>;

  /// <summary>
  /// Represents a promise-based callback for handling a bundle of chat responses.
  /// </summary>
  /// <remarks>
  /// The <c>TPromiseBundleList</c> alias extends <see cref="TPromiseCallBack&lt;TBundleList&gt;"/>
  /// to provide a promise-style API for parallel chat prompt execution, resolving with a
  /// <see cref="TBundleList"/> when all responses are complete or rejecting on error.
  /// </remarks>
  TPromiseBundleList = TPromiseCallBack<TBundleList>;

  /// <summary>
  /// Provides helper methods for managing asynchronous tasks.
  /// </summary>
  /// <remarks>
  /// This class contains utility methods for handling task execution flow,
  /// including a method to execute a follow-up action once a task completes.
  /// <para>
  /// - In order to replace TTask.WaitForAll due to a memory leak in TLightweightEvent/TCompleteEventsWrapper.
  /// See report RSP-12462 and RSP-25999.
  /// </para>
  /// </remarks>
  TTaskHelper = class
  public
    /// <summary>
    /// Executes a specified action after a given task is completed.
    /// </summary>
    /// <param name="Task">
    /// The task to wait for before executing the next action.
    /// </param>
    /// <param name="NextAction">
    /// The procedure to execute once the task is completed.
    /// </param>
    /// <param name="TimeOut">
    /// The maximum time (in milliseconds) to wait for the task to complete.
    /// The default value is 120,000 ms (2 minutes).
    /// </param>
    /// <remarks>
    /// This method waits for the specified task to finish within the provided timeout period.
    /// Once completed, the follow-up action is executed in the main thread using <c>TThread.Queue</c>,
    /// ensuring thread safety.
    /// <para>
    /// - In order to replace TTask.WaitForAll due to a memory leak in TLightweightEvent/TCompleteEventsWrapper.
    /// See report RSP-12462 and RSP-25999.
    /// </para>
    /// </remarks>
    class procedure ContinueWith(const Task: ITask; const NextAction: TProc; const TimeOut: Cardinal = 120000);
  end;

  /// <summary>
  /// Represents the parameters used for configuring a chat request bundle.
  /// </summary>
  /// <remarks>
  /// This class extends <c>TParameters</c> and provides specific methods for setting chat-related
  /// parameters, such as prompts, model selection, and reasoning effort.
  /// It is used to structure and pass multiple requests efficiently in parallel processing.
  /// </remarks>
  TBundleParams = class(TParameters)
  const
    S_PROMPT = 'prompts';
    S_SYSTEM = 'system';
    S_MODEL = 'model';
  public
    /// <summary>
    /// Sets the prompts for the chat request bundle.
    /// </summary>
    /// <param name="Value">
    /// An array of strings containing the prompts to be processed.
    /// </param>
    /// <returns>
    /// The current instance of <c>TBundleParams</c> for method chaining.
    /// </returns>
    function Prompts(const Value: TArray<string>): TBundleParams;
    /// <summary>
    /// Sets the AI model to be used for processing the chat requests.
    /// </summary>
    /// <param name="Value">
    /// A string representing the model name.
    /// </param>
    /// <returns>
    /// The current instance of <c>TBundleParams</c> for method chaining.
    /// </returns>
    function Model(const Value: string): TBundleParams;
    /// <summary>
    /// Sets the system message for the chat request bundle.
    /// </summary>
    /// <param name="Value">
    /// A string containing the system message, which provides context or behavioral instructions
    /// to guide the AI model's responses across all prompts in the bundle.
    /// </param>
    /// <returns>
    /// The current instance of <c>TBundleParams</c> to allow method chaining.
    /// </returns>
    /// <remarks>
    /// The system message is typically used to influence the tone, format, or perspective
    /// of the AI responses, acting as a global directive for the conversation context.
    /// </remarks>
    function System(const Value: string): TBundleParams;
    /// <summary>
    /// Returns prompt array
    /// </summary>
    function GetPrompt: TArray<string>;
    /// <summary>
    /// Returns system or developer instructions
    /// </summary>
    function GetSystem: string;
    /// <summary>
    /// Returns the model name
    /// </summary>
    function GetModel: string;
  end;

implementation

{ TBundleList }

function TBundleList.Add(const AIndex: Integer): TBundleItem;
begin
  Result := TBundleItem.Create;
  Result.Index := AIndex;
  FItems.Add(Result);
end;

function TBundleList.Count: Integer;
begin
  Result := FItems.Count;
end;

constructor TBundleList.Create;
begin
  inherited Create;
  FItems := TObjectList<TBundleItem>.Create(True);
end;

destructor TBundleList.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TBundleList.Item(const AIndex: Integer): TBundleItem;
begin
  if (AIndex < 0) or (AIndex > Pred(Count)) then
    raise Exception.Create('Index out of bounds');
  Result := FItems.Items[AIndex];
end;

{ TTaskHelper }

class procedure TTaskHelper.ContinueWith(const Task: ITask;
  const NextAction: TProc; const TimeOut: Cardinal);
begin
  TTask.Run(
    procedure
    begin
      {--- Wait for the task to complete within TimeOut ms }
      Task.Wait(TimeOut);

      {--- Execute the sequence in the main thread }
      TThread.Queue(nil,
        procedure
        begin
          NextAction();
        end);
    end);
end;

{ TBundleParams }

function TBundleParams.GetModel: string;
begin
  Result := GetString(S_MODEL);
end;

function TBundleParams.GetPrompt: TArray<string>;
begin
  Result := GetArrayString(S_PROMPT);
end;

function TBundleParams.GetSystem: string;
begin
  Result := GetString(S_SYSTEM);
end;

function TBundleParams.Model(const Value: string): TBundleParams;
begin
  Result := TBundleParams(Add(S_MODEL, Value));
end;

function TBundleParams.Prompts(const Value: TArray<string>): TBundleParams;
begin
  Result := TBundleParams(Add(S_PROMPT, Value));
end;

function TBundleParams.System(const Value: string): TBundleParams;
begin
  Result := TBundleParams(Add(S_SYSTEM, Value));
end;

{ TBundleItem }

destructor TBundleItem.Destroy;
begin
  if Assigned(FChat) then
    FChat.Free;
  inherited;
end;

end.
