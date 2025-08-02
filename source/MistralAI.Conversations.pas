unit MistralAI.Conversations;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiMistralAI
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, System.Threading,
  MistralAI.API.Params, MistralAI.API, MistralAI.Types, MistralAI.Conversations.Params,
  MistralAI.Async.Params, MistralAI.Async.Support, MistralAI.Async.Promise,
  MistralAI.Conversations.Chunks, MistralAI.Conversations.Manager,
  MistralAI.Conversations.EventStreaming, MistralAI.Conversations.Internal,
  MistralAI.Async.Parallel;

type
  /// <summary>
  /// Provides a high‑level interface for managing conversation resources.
  /// </summary>
  /// <remarks>
  /// Inherits from <see cref="TConversationsRouteInternal"/> and exposes synchronous,
  /// asynchronous, and streaming methods to create, append, restart, list, retrieve,
  /// and stream conversation events. Designed for seamless integration with the
  /// MistralAI Conversations API, it centralizes operations on conversations and
  /// handles request/response normalization, streaming event parsing, and error handling.
  /// </remarks>
  TConversationsRoute =  class(TConversationsRouteInternal)
  public
    /// <summary>
    /// Initiates an asynchronous "create conversation" request and returns a promise for the resulting conversation.
    /// </summary>
    /// <remarks>
    /// This method wraps the callback‑based <c>AsyncCreate</c> invocation in a promise interface.
    /// It sends the configured parameters to the "conversations" endpoint and resolves when the full
    /// <see cref="TConversation"/> object is available, or rejects on error.
    /// </remarks>
    /// <param name="ParamProc">
    /// A procedure that configures the <see cref="TConversationsParams"/>, including model selection,
    /// prompt text, and any additional options such as stop sequences or maximum tokens.
    /// </param>
    /// <param name="Callbacks">
    /// Optional. A function returning a <see cref="TPromiseConversation"/> that allows hooking into
    /// <c>OnStart</c>, <c>OnProgress</c>, <c>OnSuccess</c>, and <c>OnError</c> events.
    /// </param>
    /// <returns>
    /// A <see cref="TPromise{TConversation}"/> that resolves with the created <see cref="TConversation"/>
    /// when the API call completes successfully, or rejects with an exception on failure.
    /// </returns>
    function AsyncAwaitCreate(
      const ParamProc: TProc<TConversationsParams>;
      const Callbacks: TFunc<TPromiseConversation> = nil): TPromise<TConversation>;

    /// <summary>
    /// Initiates parallel processing of multiple chat prompts and returns a promise for the bundled results.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to configure the bundle parameters (model, prompts, reasoning effort, etc.) before dispatch.
    /// </param>
    /// <param name="CallBacks">
    /// An optional function that provides a <see cref="TPromiseBundleList"/> record for hooking into lifecycle events.
    /// </param>
    /// <returns>
    /// A <see cref="TPromise&lt;TBundleList&gt;"/> that resolves with a <see cref="TBundleList"/> containing all responses
    /// once every parallel task completes, or rejects on the first error.
    /// </returns>
    /// <remarks>
    /// This method wraps the callback-based <see cref="CreateParallel"/> in a promise interface, enabling async/await usage
    /// for parallel prompt execution. If <paramref name="CallBacks"/> is omitted, only the promise resolution is available.
    /// </remarks>
    function AsyncAwaitCreateParallel(const ParamProc: TProc<TBundleParams>;
      const CallBacks: TFunc<TPromiseBundleList> = nil): TPromise<TBundleList>;

    /// <summary>
    /// Initiates an asynchronous streaming "create conversation" request and returns a promise for the concatenated response text.
    /// </summary>
    /// <remarks>
    /// This method wraps the callback‑based <c>AsyncCreateStream</c> invocation into a promise interface.
    /// It sends the configured parameters to the "conversations#stream" endpoint and collects each
    /// chunk of the server‑sent events (SSE) stream into a single string. The promise resolves when
    /// the stream ends or rejects on error.
    /// </remarks>
    /// <param name="ParamProc">
    /// A procedure that configures the <see cref="TConversationsParams"/>, including model selection,
    /// prompt text, and any streaming options such as maximum tokens or stop sequences.
    /// </param>
    /// <param name="Callbacks">
    /// Optional. A function returning a <see cref="TPromiseConversationsEvent"/> that allows hooking into
    /// <c>OnStart</c>, <c>OnProgress</c>, <c>OnError</c>, and <c>OnDoCancel</c> events for each stream chunk.
    /// </param>
    /// <returns>
    /// A <see cref="TPromise{String}"/> that resolves with the full concatenated text of all chunks when
    /// the stream completes, or rejects with an exception if an error occurs or the stream is aborted.
    /// </returns>
    function AsyncAwaitCreateStream(
      const ParamProc: TProc<TConversationsParams>;
      const Callbacks: TFunc<TPromiseConversationsEvent> = nil): TPromise<string>;

    /// <summary>
    /// Initiates an asynchronous streaming "append to conversation" request and returns a promise for the concatenated response text.
    /// </summary>
    /// <remarks>
    /// This method wraps the callback‑based <c>AsyncAppendStream</c> invocation in a promise interface.
    /// It sends the configured parameters to the "conversations/{ConversationId}#stream" endpoint,
    /// processes each server‑sent event (SSE) chunk, and accumulates the text into a single string.
    /// The promise resolves when the stream ends or rejects on error.
    /// </remarks>
    /// <param name="ConversationId">
    /// The identifier of the conversation to which new input is appended.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure that configures the <see cref="TConversationsParams"/>, including the new message content,
    /// model settings, and any streaming options such as stop sequences or maximum tokens.
    /// </param>
    /// <param name="Callbacks">
    /// Optional. A function returning a <see cref="TPromiseConversationsEvent"/> that allows hooking into
    /// <c>OnStart</c>, <c>OnProgress</c>, <c>OnError</c>, and <c>OnDoCancel</c> events for each incoming chunk.
    /// </param>
    /// <returns>
    /// A <see cref="TPromise{String}"/> that resolves with the full concatenated text of all SSE chunks when
    /// the append stream completes, or rejects with an exception if an error occurs or the stream is aborted.
    /// </returns>
    function AsyncAwaitAppendStream(
      const ConversationId: string;
      const ParamProc: TProc<TConversationsParams>;
      const Callbacks: TFunc<TPromiseConversationsEvent> = nil): TPromise<string>;

    /// <summary>
    /// Initiates an asynchronous streaming "restart conversation" request and returns a promise for the concatenated response text.
    /// </summary>
    /// <remarks>
    /// This method wraps the callback‑based <c>AsyncRestartStream</c> invocation in a promise interface.
    /// It sends the configured parameters to the "conversations/{ConversationId}/restart#stream" endpoint,
    /// processes each server‑sent event (SSE) chunk, and accumulates the text into a single string.
    /// The promise resolves when the stream ends or rejects on error.
    /// </remarks>
    /// <param name="ConversationId">
    /// The identifier of the conversation to restart; the new stream will begin from the conversation’s start.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure that configures the <see cref="TConversationsParams"/>, including the initial prompt,
    /// model settings, and any streaming options such as stop sequences or maximum tokens.
    /// </param>
    /// <param name="Callbacks">
    /// Optional. A function returning a <see cref="TPromiseConversationsEvent"/> that allows hooking into
    /// <c>OnStart</c>, <c>OnProgress</c>, <c>OnError</c>, and <c>OnDoCancel</c> events for each stream chunk.
    /// </param>
    /// <returns>
    /// A <see cref="TPromise{String}"/> that resolves with the full concatenated text of all SSE chunks when
    /// the restart stream completes, or rejects with an exception if an error occurs or the stream is aborted.
    /// </returns>
    function AsyncAwaitRestartStream(
      const ConversationId: string;
      const ParamProc: TProc<TConversationsParams>;
      const Callbacks: TFunc<TPromiseConversationsEvent> = nil): TPromise<string>;

    /// <summary>
    /// Initiates an asynchronous "append to conversation" request and returns a promise for the updated conversation.
    /// </summary>
    /// <remarks>
    /// This method wraps the callback‑based <c>AsyncAppend</c> invocation into a promise interface.
    /// It sends the configured parameters to the "conversations/{ConversationId}" endpoint and resolves with the
    /// full <see cref="TConversation"/> object once the append operation completes, or rejects on error.
    /// </remarks>
    /// <param name="ConversationId">
    /// The identifier of the existing conversation to which the new input will be appended.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure that configures the <see cref="TConversationsParams"/>, including the message content,
    /// model selection, and any additional options such as stop sequences or maximum tokens.
    /// </param>
    /// <param name="Callbacks">
    /// Optional. A function returning a <see cref="TPromiseConversation"/> that allows hooking into
    /// <c>OnStart</c>, <c>OnProgress</c>, <c>OnSuccess</c>, and <c>OnError</c> events.
    /// </param>
    /// <returns>
    /// A <see cref="TPromise{TConversation}"/> that resolves with the updated <see cref="TConversation"/>
    /// when the API call completes successfully, or rejects with an exception on failure.
    /// </returns>
    function AsyncAwaitAppend(
      const ConversationId: string;
      const ParamProc: TProc<TConversationsParams>;
      const Callbacks: TFunc<TPromiseConversation> = nil): TPromise<TConversation>;

    /// <summary>
    /// Initiates an asynchronous "list conversations" request and returns a promise for the conversation list.
    /// </summary>
    /// <remarks>
    /// This method wraps the callback‑based <c>AsyncList</c> invocation into a promise interface.
    /// It sends the configured parameters to the "conversations" endpoint and resolves with a
    /// <see cref="TConversationsList"/> containing metadata and entries, or rejects on error.
    /// </remarks>
    /// <param name="ParamProc">
    /// A procedure that configures the <see cref="TConversationsListParams"/>, including pagination,
    /// filtering, or sorting options for the returned conversations.
    /// </param>
    /// <param name="Callbacks">
    /// Optional. A function returning a <see cref="TPromiseConversationsList"/> that allows hooking
    /// into <c>OnStart</c>, <c>OnSuccess</c>, and <c>OnError</c> events.
    /// </param>
    /// <returns>
    /// A <see cref="TPromise{TConversationsList}"/> that resolves with the retrieved
    /// <see cref="TConversationsList"/> when the API call completes successfully, or rejects with
    /// an exception on failure.
    /// </returns>
    function AsyncAwaitList(
      const ParamProc: TProc<TConversationsListParams>;
      const Callbacks: TFunc<TPromiseConversationsList> = nil): TPromise<TConversationsList>;

    /// <summary>
    /// Initiates an asynchronous request to retrieve a specific conversation and returns a promise for the conversation item.
    /// </summary>
    /// <remarks>
    /// This method wraps the callback‑based <c>AsyncRetrieve</c> invocation into a promise interface.
    /// It sends a GET request to the "conversations/{ConversationId}" endpoint and resolves with the
    /// <see cref="TConversationsListItem"/> containing the conversation metadata and messages, or rejects on error.
    /// </remarks>
    /// <param name="ConversationId">
    /// The identifier of the conversation to retrieve.
    /// </param>
    /// <param name="Callbacks">
    /// Optional. A function returning a <see cref="TPromiseConversationsListItem"/> that allows hooking into
    /// <c>OnStart</c>, <c>OnSuccess</c>, and <c>OnError</c> events for the retrieval operation.
    /// </param>
    /// <returns>
    /// A <see cref="TPromise{TConversationsListItem}"/> that resolves with the requested
    /// <see cref="TConversationsListItem"/> when the API call completes successfully, or rejects with an exception on failure.
    /// </returns>
    function AsyncAwaitRetrieve(
      const ConversationId: string;
      const Callbacks: TFunc<TPromiseConversationsListItem> = nil): TPromise<TConversationsListItem>;

    /// <summary>
    /// Initiates an asynchronous request to fetch the full history of a conversation and returns a promise for the entries.
    /// </summary>
    /// <remarks>
    /// This method wraps the callback‑based <c>AsyncGetHistory</c> into a promise interface.
    /// It issues a GET request to the "conversations/{ConversationId}/history" endpoint and resolves with
    /// a <see cref="TRetrievedEntries"/> containing all entries (messages, function calls, etc.), or rejects on error.
    /// </remarks>
    /// <param name="ConversationId">
    /// The identifier of the conversation whose history is being retrieved.
    /// </param>
    /// <param name="Callbacks">
    /// Optional. A function returning a <see cref="TPromiseRetrievedEntries"/> that allows hooking into
    /// <c>OnStart</c>, <c>OnSuccess</c>, and <c>OnError</c> events for the retrieval process.
    /// </param>
    /// <returns>
    /// A <see cref="TPromise{TRetrievedEntries}"/> that resolves with the complete conversation history
    /// when the API call completes successfully, or rejects with an exception on failure.
    /// </returns>
    function AsyncAwaitGetHistory(
      const ConversationId: string;
      const Callbacks: TFunc<TPromiseRetrievedEntries> = nil): TPromise<TRetrievedEntries>;

    /// <summary>
    /// Initiates an asynchronous request to fetch all messages of a conversation and returns a promise for the messages.
    /// </summary>
    /// <remarks>
    /// This method wraps the callback‑based <c>AsyncGetMessages</c> into a promise interface.
    /// It issues a GET request to the "conversations/{ConversationId}/messages" endpoint and resolves with
    /// a <see cref="TRetrieveMessages"/> containing only the message entries, or rejects on error.
    /// </remarks>
    /// <param name="ConversationId">
    /// The identifier of the conversation whose messages are being retrieved.
    /// </param>
    /// <param name="Callbacks">
    /// Optional. A function returning a <see cref="TPromiseRetrieveMessages"/> that allows hooking into
    /// <c>OnStart</c>, <c>OnSuccess</c>, and <c>OnError</c> events for the message retrieval operation.
    /// </param>
    /// <returns>
    /// A <see cref="TPromise{TRetrieveMessages}"/> that resolves with the conversation’s messages
    /// when the API call completes successfully, or rejects with an exception on failure.
    /// </returns>
    function AsyncAwaitGetMessages(
      const ConversationId: string;
      const Callbacks: TFunc<TPromiseRetrieveMessages> = nil): TPromise<TRetrieveMessages>;

    /// <summary>
    /// Initiates an asynchronous "restart conversation" request and returns a promise for the restarted conversation.
    /// </summary>
    /// <remarks>
    /// This method wraps the callback‑based <c>AsyncRestart</c> invocation into a promise interface.
    /// It sends the configured parameters to the "conversations/{ConversationId}/restart" endpoint and
    /// resolves with a fresh <see cref="TConversation"/> object that reflects the conversation restarted
    /// from its initial state, or rejects on error.
    /// </remarks>
    /// <param name="ConversationId">
    /// The identifier of the conversation to restart.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure that configures the <see cref="TConversationsParams"/>, including the initial prompt,
    /// model selection, and any options such as stop sequences or maximum tokens.
    /// </param>
    /// <param name="Callbacks">
    /// Optional. A function returning a <see cref="TPromiseConversation"/> that allows hooking into
    /// <c>OnStart</c>, <c>OnProgress</c>, <c>OnSuccess</c>, and <c>OnError</c> events.
    /// </param>
    /// <returns>
    /// A <see cref="TPromise{TConversation}"/> that resolves with the restarted <see cref="TConversation"/>
    /// when the API call completes successfully, or rejects with an exception on failure.
    /// </returns>
    function AsyncAwaitRestart(
      const ConversationId: string;
      const ParamProc: TProc<TConversationsParams>;
      const Callbacks: TFunc<TPromiseConversation> = nil): TPromise<TConversation>;

    /// <summary>
    /// Sends a synchronous "create conversation" request and returns the resulting conversation.
    /// </summary>
    /// <remarks>
    /// This method issues a POST to the "conversations" endpoint using the configured parameters.
    /// It blocks until the API returns a <see cref="TConversation"/> object or raises an exception on failure.
    /// </remarks>
    /// <param name="ParamProc">
    /// A procedure that configures the <see cref="TConversationsParams"/>, including model selection,
    /// prompt text, and any additional options such as stop sequences or maximum tokens.
    /// </param>
    /// <returns>
    /// The newly created <see cref="TConversation"/> containing the conversation ID and initial outputs.
    /// </returns>
    function Create(const ParamProc: TProc<TConversationsParams>): TConversation;

    /// <summary>
    /// Initiates parallel processing of chat prompts by creating multiple chat completions
    /// asynchronously, with results stored in a bundle and provided back to the callback function.
    /// This method allows for parallel processing of multiple prompts in an efficient manner,
    /// handling errors and successes for each chat completion.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure delegate that configures the parameters for the bundle. It is responsible
    /// for providing the necessary settings (such as model and reasoning effort) for the chat completions.
    /// </param>
    /// <param name="CallBacks">
    /// A function that returns an instance of TAsynBuffer, which manages the lifecycle of the
    /// asynchronous operation. The callbacks include handlers for start, error, and success events.
    /// </param>
    /// <remarks>
    /// The method allows for efficient parallel processing of multiple prompts by delegating
    /// individual tasks to separate threads. It each task's result is properly bundled and communicated back to the caller.
    /// If an error occurs, the error handling callback will be triggered, and the rest of the tasks
    /// will continue processing. The success callback is triggered once all tasks are completed.
    /// </remarks>
    procedure CreateParallel(const ParamProc: TProc<TBundleParams>;
      const CallBacks: TFunc<TAsynBundleList>);

    /// <summary>
    /// Sends a synchronous streaming "create conversation" request and returns a success flag.
    /// </summary>
    /// <remarks>
    /// This method issues a POST to the "conversations#stream" endpoint using the configured parameters
    /// and invokes the provided <see cref="TConversationsEventRef"/> callback for each server‑sent event chunk.
    /// It blocks until the stream completes or an unrecoverable error occurs.
    /// </remarks>
    /// <param name="ParamProc">
    /// A procedure that configures the <see cref="TConversationsParams"/>, including model selection,
    /// prompt text, and any streaming options such as maximum tokens or stop sequences.
    /// </param>
    /// <param name="Event">
    /// A reference callback (<see cref="TConversationsEventRef"/>) invoked for each <see cref="TConversationsEvent"/> chunk.
    /// Use the <c>IsDone</c> flag to detect end‑of‑stream and set <c>Cancel</c> to abort prematurely.
    /// </param>
    /// <returns>
    /// <c>True</c> if the stream completed successfully or was cancelled via the callback;
    /// <c>False</c> if an unrecoverable error occurred during HTTP transfer.
    /// </returns>
    function CreateStream(
      const ParamProc: TProc<TConversationsParams>;
      const Event: TConversationsEventRef): Boolean;

    /// <summary>
    /// Sends a synchronous streaming "append to conversation" request and returns a success flag.
    /// </summary>
    /// <remarks>
    /// This method issues a POST to the "conversations/{ConversationId}#stream" endpoint using the configured parameters
    /// and invokes the provided <see cref="TConversationsEventRef"/> callback for each server‑sent event (SSE) chunk.
    /// It blocks until the stream completes or an unrecoverable error occurs.
    /// </remarks>
    /// <param name="ConversationId">
    /// The identifier of the conversation to which new input is appended.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure that configures the <see cref="TConversationsParams"/>, including the appended message content,
    /// model settings, and any streaming options such as stop sequences or maximum tokens.
    /// </param>
    /// <param name="Event">
    /// A reference callback (<see cref="TConversationsEventRef"/>) invoked for each <see cref="TConversationsEvent"/> chunk.
    /// The <c>IsDone</c> flag indicates end‑of‑stream; setting <c>Cancel</c> to <c>True</c> aborts the stream.
    /// </param>
    /// <returns>
    /// <c>True</c> if the stream completed successfully or was cancelled via the callback;
    /// <c>False</c> if an unrecoverable error occurred during HTTP transfer.
    /// </returns>
    function AppendStream(
      const ConversationId: string;
      const ParamProc: TProc<TConversationsParams>;
      const Event: TConversationsEventRef): Boolean; overload;

    /// <summary>
    /// Sends a synchronous streaming "restart conversation" request and returns a success flag.
    /// </summary>
    /// <remarks>
    /// This method issues a POST to the "conversations/{ConversationId}/restart#stream" endpoint using the configured parameters
    /// and invokes the provided <see cref="TConversationsEventRef"/> callback for each server‑sent event (SSE) chunk.
    /// It blocks until the stream completes or an unrecoverable error occurs.
    /// </remarks>
    /// <param name="ConversationId">
    /// The identifier of the conversation to restart; the stream will begin from the conversation’s start.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure that configures the <see cref="TConversationsParams"/>, including the initial prompt,
    /// model settings, and any streaming options such as stop sequences or maximum tokens.
    /// </param>
    /// <param name="Event">
    /// A reference callback (<see cref="TConversationsEventRef"/>) invoked for each <see cref="TConversationsEvent"/> chunk.
    /// The <c>IsDone</c> flag indicates end‑of‑stream; setting <c>Cancel</c> to <c>True</c> aborts the stream.
    /// </param>
    /// <returns>
    /// <c>True</c> if the stream completed successfully or was cancelled via the callback;
    /// <c>False</c> if an unrecoverable error occurred during HTTP transfer.
    /// </returns>
    function RestartStream(
      const ConversationId: string;
      const ParamProc: TProc<TConversationsParams>;
      const Event: TConversationsEventRef): Boolean; overload;

    /// <summary>
    /// Sends a synchronous "append to conversation" request and returns the updated conversation.
    /// </summary>
    /// <remarks>
    /// This method issues a POST to the "conversations/{ConversationId}" endpoint using the configured parameters.
    /// It blocks until the API returns the full <see cref="TConversation"/> object reflecting the appended input,
    /// or raises an exception on failure.
    /// </remarks>
    /// <param name="ConversationId">
    /// The identifier of the conversation to which new input will be appended.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure that configures the <see cref="TConversationsParams"/>, including the message content,
    /// model selection, and any additional options such as stop sequences or maximum tokens.
    /// </param>
    /// <returns>
    /// The updated <see cref="TConversation"/> containing the full conversation state after the append operation.
    /// </returns>
    function Append(
      const ConversationId: string;
      const ParamProc: TProc<TConversationsParams>): TConversation;

    /// <summary>
    /// Sends a synchronous "list conversations" request and returns the conversation list.
    /// </summary>
    /// <remarks>
    /// This method issues a GET to the "conversations" endpoint using the configured parameters.
    /// It blocks until the API returns a <see cref="TConversationsList"/> containing conversation metadata
    /// and entries, or raises an exception on failure.
    /// </remarks>
    /// <param name="ParamProc">
    /// A procedure that configures the <see cref="TConversationsListParams"/>, including pagination,
    /// filtering, or sorting options for the returned conversations.
    /// </param>
    /// <returns>
    /// A <see cref="TConversationsList"/> containing the retrieved conversations and their metadata.
    /// </returns>
    function List(const ParamProc: TProc<TConversationsListParams>): TConversationsList;

    /// <summary>
    /// Sends a synchronous request to retrieve a specific conversation and returns the conversation item.
    /// </summary>
    /// <remarks>
    /// This method issues a GET to the "conversations/{ConversationId}" endpoint using the provided identifier.
    /// It blocks until the API returns a <see cref="TConversationsListItem"/> containing the conversation’s
    /// metadata and messages, or raises an exception on failure.
    /// </remarks>
    /// <param name="ConversationId">
    /// The identifier of the conversation to retrieve.
    /// </param>
    /// <returns>
    /// A <see cref="TConversationsListItem"/> representing the retrieved conversation,
    /// including its messages and associated metadata.
    /// </returns>
    function Retrieve(const ConversationId: string): TConversationsListItem;

    /// <summary>
    /// Sends a synchronous request to fetch the full history of a conversation and returns all entries.
    /// </summary>
    /// <remarks>
    /// This method issues a GET to the "conversations/{ConversationId}/history" endpoint using the provided identifier.
    /// It blocks until the API returns a <see cref="TRetrievedEntries"/> object containing every entry
    /// (messages, function calls, etc.) in the conversation, or raises an exception on failure.
    /// </remarks>
    /// <param name="ConversationId">
    /// The identifier of the conversation whose history is being retrieved.
    /// </param>
    /// <returns>
    /// A <see cref="TRetrievedEntries"/> containing the complete sequence of entries for the specified conversation.
    /// </returns>
    function GetHistory(const ConversationId: string): TRetrievedEntries;

    /// <summary>
    /// Sends a synchronous request to fetch all messages of a conversation and returns the message entries.
    /// </summary>
    /// <remarks>
    /// This method issues a GET to the "conversations/{ConversationId}/messages" endpoint using the provided identifier.
    /// It blocks until the API returns a <see cref="TRetrieveMessages"/> object containing all message entries,
    /// or raises an exception on failure.
    /// </remarks>
    /// <param name="ConversationId">
    /// The identifier of the conversation whose messages are being retrieved.
    /// </param>
    /// <returns>
    /// A <see cref="TRetrieveMessages"/> containing the list of message entries for the specified conversation.
    /// </returns>
    function GetMessages(const ConversationId: string): TRetrieveMessages;

    /// <summary>
    /// Sends a synchronous "restart conversation" request and returns the restarted conversation.
    /// </summary>
    /// <remarks>
    /// This method issues a POST to the "conversations/{ConversationId}/restart" endpoint using the configured parameters.
    /// It blocks until the API returns a <see cref="TConversation"/> object reflecting the conversation restarted
    /// from its initial state, or raises an exception on failure.
    /// </remarks>
    /// <param name="ConversationId">
    /// The identifier of the conversation to restart.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure that configures the <see cref="TConversationsParams"/>, including the initial prompt,
    /// model settings, and any options such as stop sequences or maximum tokens.
    /// </param>
    /// <returns>
    /// The newly created <see cref="TConversation"/> representing the restarted conversation.
    /// </returns>
    function Restart(
      const ConversationId: string;
      const ParamProc: TProc<TConversationsParams>): TConversation;

    /// <summary>
    /// Initiates an asynchronous "create conversation" request using callbacks.
    /// </summary>
    /// <remarks>
    /// This method sends the configured parameters to the "conversations" endpoint and invokes
    /// the provided callback handlers for lifecycle events. It uses <c>AsyncCreate</c> internally.
    /// </remarks>
    /// <param name="ParamProc">
    /// A procedure that configures the <see cref="TConversationsParams"/>, including model selection,
    /// prompt text, and any additional options such as stop sequences or maximum tokens.
    /// </param>
    /// <param name="Callbacks">
    /// A function returning a <see cref="TAsyncConversation"/>. Use the returned object’s
    /// <c>OnStart</c> event to respond when the request begins, <c>OnSuccess</c> to handle
    /// the resulting <see cref="TConversation"/> on success, and <c>OnError</c> to handle any
    /// error message if the request fails.
    /// </param>
    procedure AsyncCreate(
      const ParamProc: TProc<TConversationsParams>;
      const Callbacks: TFunc<TAsyncConversation>);

    /// <summary>
    /// Initiates an asynchronous streaming "create conversation" request using callbacks.
    /// </summary>
    /// <remarks>
    /// This method sends the configured parameters to the "conversations#stream" endpoint and invokes
    /// the provided callback handlers for each server‑sent event chunk. It uses <c>AsyncCreateStream</c> internally.
    /// </remarks>
    /// <param name="ParamProc">
    /// A procedure that configures the <see cref="TConversationsParams"/>, including model selection,
    /// prompt text, and any streaming options such as maximum tokens or stop sequences.
    /// </param>
    /// <param name="Callbacks">
    /// A function returning a <see cref="TAsyncConversationsEvent"/>. Use the returned object’s
    /// <c>OnStart</c> event to respond when the stream begins, <c>OnProgress</c> to handle each
    /// <see cref="TConversationsEvent"/> chunk, <c>OnError</c> to handle any errors during streaming,
    /// and <c>OnDoCancel</c> to cooperatively abort the stream if needed.
    /// </param>
    procedure AsyncCreateStream(
      const ParamProc: TProc<TConversationsParams>;
      const Callbacks: TFunc<TAsyncConversationsEvent>);

    /// <summary>
    /// Initiates an asynchronous streaming "append to conversation" request using callbacks.
    /// </summary>
    /// <remarks>
    /// This method sends the configured parameters to the "conversations/{ConversationId}#stream" endpoint and invokes
    /// the provided callback handlers for each server‑sent event chunk. It uses <c>AsyncAppendStream</c> internally.
    /// </remarks>
    /// <param name="ConversationId">
    /// The identifier of the existing conversation to which new input is appended.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure that configures the <see cref="TConversationsParams"/>, including the appended message content,
    /// model selection, and any streaming options such as stop sequences or maximum tokens.
    /// </param>
    /// <param name="Callbacks">
    /// A function returning a <see cref="TAsyncConversationsEvent"/>. Use the returned object’s <c>OnStart</c> event
    /// to respond when the stream begins, <c>OnProgress</c> to handle each <see cref="TConversationsEvent"/> chunk,
    /// <c>OnError</c> to handle any streaming errors, and <c>OnDoCancel</c> to cooperatively abort the stream if needed.
    /// </param>
    procedure AsyncAppendStream(
      const ConversationId: string;
      const ParamProc: TProc<TConversationsParams>;
      const Callbacks: TFunc<TAsyncConversationsEvent>);

    /// <summary>
    /// Initiates an asynchronous streaming "restart conversation" request using callbacks.
    /// </summary>
    /// <remarks>
    /// This method sends the configured parameters to the "conversations/{ConversationId}/restart#stream" endpoint
    /// and invokes the provided callback handlers for each server‑sent event chunk. It uses <c>AsyncRestartStream</c> internally.
    /// </remarks>
    /// <param name="ConversationId">
    /// The identifier of the conversation to restart; the stream will begin from the conversation’s initial state.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure that configures the <see cref="TConversationsParams"/>, including the initial prompt,
    /// model selection, and any streaming options such as stop sequences or maximum tokens.
    /// </param>
    /// <param name="Callbacks">
    /// A function returning a <see cref="TAsyncConversationsEvent"/>. Use the returned object’s
    /// <c>OnStart</c> event to respond when the stream begins, <c>OnProgress</c> to handle each
    /// <see cref="TConversationsEvent"/> chunk, <c>OnError</c> to handle any streaming errors,
    /// and <c>OnDoCancel</c> to cooperatively abort the stream if needed.
    /// </param>
    procedure AsyncRestartStream(
      const ConversationId: string;
      const ParamProc: TProc<TConversationsParams>;
      const Callbacks: TFunc<TAsyncConversationsEvent>);

    /// <summary>
    /// Initiates an asynchronous "append to conversation" request using callbacks.
    /// </summary>
    /// <remarks>
    /// This method sends the configured parameters to the "conversations/{ConversationId}" endpoint
    /// and invokes the provided callback handlers for lifecycle events. It uses <c>AsyncAppend</c> internally.
    /// </remarks>
    /// <param name="ConversationId">
    /// The identifier of the existing conversation to which new input will be appended.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure that configures the <see cref="TConversationsParams"/>, including the appended message content,
    /// model selection, and any additional options such as stop sequences or maximum tokens.
    /// </param>
    /// <param name="Callbacks">
    /// A function returning a <see cref="TAsyncConversation"/>. Use the returned object’s <c>OnStart</c> event
    /// to respond when the request begins, <c>OnSuccess</c> to handle the resulting <see cref="TConversation"/> on success,
    /// and <c>OnError</c> to handle any error message if the request fails.
    /// </param>
    procedure AsyncAppend(
      const ConversationId: string;
      const ParamProc: TProc<TConversationsParams>;
      const Callbacks: TFunc<TAsyncConversation>);

    /// <summary>
    /// Initiates an asynchronous "list conversations" request using callbacks.
    /// </summary>
    /// <remarks>
    /// This method sends the configured parameters to the "conversations" endpoint and invokes
    /// the provided callback handlers for lifecycle events. It uses <c>AsyncList</c> internally.
    /// </remarks>
    /// <param name="ParamProc">
    /// A procedure that configures the <see cref="TConversationsListParams"/>, including pagination,
    /// filtering, or sorting options for the returned conversations.
    /// </param>
    /// <param name="Callbacks">
    /// A function returning a <see cref="TAsyncConversationsList"/>. Use the returned object’s
    /// <c>OnStart</c> event to respond when the request begins, <c>OnSuccess</c> to handle the resulting
    /// <see cref="TConversationsList"/> on success, and <c>OnError</c> to handle any error if the request fails.
    /// </param>
    procedure AsyncList(
      const ParamProc: TProc<TConversationsListParams>;
      const Callbacks: TFunc<TAsyncConversationsList>);

    /// <summary>
    /// Initiates an asynchronous request to retrieve a specific conversation using callbacks.
    /// </summary>
    /// <remarks>
    /// This method sends a GET request to the "conversations/{ConversationId}" endpoint and invokes
    /// the provided callback handlers for lifecycle events. It uses <c>AsyncRetrieve</c> internally.
    /// </remarks>
    /// <param name="ConversationId">
    /// The identifier of the conversation to retrieve.
    /// </param>
    /// <param name="Callbacks">
    /// A function returning a <see cref="TAsyncConversationsListItem"/>. Use the returned object’s
    /// <c>OnStart</c> event to respond when the request begins, <c>OnSuccess</c> to handle the resulting
    /// <see cref="TConversationsListItem"/> on success, and <c>OnError</c> to handle any error if the request fails.
    /// </param>
    procedure AsyncRetrieve(
      const ConversationId: string;
      const Callbacks: TFunc<TAsyncConversationsListItem>);

    /// <summary>
    /// Initiates an asynchronous request to fetch the full history of a conversation using callbacks.
    /// </summary>
    /// <remarks>
    /// This method sends a GET request to the "conversations/{ConversationId}/history" endpoint and invokes
    /// the provided callback handlers for lifecycle events. It uses <c>AsyncGetHistory</c> internally.
    /// </remarks>
    /// <param name="ConversationId">
    /// The identifier of the conversation whose history is being retrieved.
    /// </param>
    /// <param name="Callbacks">
    /// A function returning a <see cref="TAsyncRetrievedEntries"/>. Use the returned object’s <c>OnStart</c> event
    /// to respond when the request begins, <c>OnSuccess</c> to handle the resulting <see cref="TRetrievedEntries"/>
    /// on success, and <c>OnError</c> to handle any error if the request fails.
    /// </param>
    procedure AsyncGetHistory(
      const ConversationId: string;
      const Callbacks: TFunc<TAsyncRetrievedEntries>);

    /// <summary>
    /// Initiates an asynchronous request to fetch all messages of a conversation using callbacks.
    /// </summary>
    /// <remarks>
    /// This method sends a GET request to the "conversations/{ConversationId}/messages" endpoint and invokes
    /// the provided callback handlers for lifecycle events. It uses <c>AsyncGetMessages</c> internally.
    /// </remarks>
    /// <param name="ConversationId">
    /// The identifier of the conversation whose messages are being retrieved.
    /// </param>
    /// <param name="Callbacks">
    /// A function returning a <see cref="TAsyncRetrieveMessages"/>. Use the returned object’s <c>OnStart</c> event
    /// to respond when the request begins, <c>OnSuccess</c> to handle the resulting <see cref="TRetrieveMessages"/>
    /// on success, and <c>OnError</c> to handle any error if the request fails.
    /// </param>
    procedure AsyncGetMessages(
      const ConversationId: string;
      const Callbacks: TFunc<TAsyncRetrieveMessages>);

    /// <summary>
    /// Initiates an asynchronous "restart conversation" request using callbacks.
    /// </summary>
    /// <remarks>
    /// This method sends the configured parameters to the "conversations/{ConversationId}/restart" endpoint
    /// and invokes the provided callback handlers for lifecycle events. It uses <c>AsyncRestart</c> internally.
    /// </remarks>
    /// <param name="ConversationId">
    /// The identifier of the conversation to restart.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure that configures the <see cref="TConversationsParams"/>, including the initial prompt,
    /// model selection, and any options such as stop sequences or maximum tokens.
    /// </param>
    /// <param name="Callbacks">
    /// A function returning a <see cref="TAsyncConversation"/>. Use the returned object’s <c>OnStart</c> event
    /// to respond when the request begins, <c>OnSuccess</c> to handle the resulting <see cref="TConversation"/> on success,
    /// and <c>OnError</c> to handle any error message if the request fails.
    /// </param>
    procedure AsyncRestart(
      const ConversationId: string;
      const ParamProc: TProc<TConversationsParams>;
      const Callbacks: TFunc<TAsyncConversation>);
  end;

implementation

{ TConversationsRoute }

function TConversationsRoute.Append(
  const ConversationId: string;
  const ParamProc: TProc<TConversationsParams>): TConversation;
begin
  Result := API.Post<TConversation, TConversationsParams>('conversations/' + ConversationId, ParamProc,
    ['outputs', '*', 'content']);
end;

function TConversationsRoute.AppendStream(
  const ConversationId: string;
  const ParamProc: TProc<TConversationsParams>;
  const Event: TConversationsEventRef): Boolean;
begin
  Result := CreateStreamInternal('conversations/' + ConversationId + '#stream', ParamProc, Event);
end;

procedure TConversationsRoute.AsyncAppend(
  const ConversationId: string;
  const ParamProc: TProc<TConversationsParams>;
  const Callbacks: TFunc<TAsyncConversation>);
begin
  with TAsyncCallbackExec<TAsyncConversation, TConversation>.Create(Callbacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TConversation
      begin
        Result := Self.Append(ConversationId, ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TConversationsRoute.AsyncAppendStream(
  const ConversationId: string;
  const ParamProc: TProc<TConversationsParams>;
  const Callbacks: TFunc<TAsyncConversationsEvent>);
begin
  AsyncStreamInternal('conversations/' + ConversationId + '#stream', ParamProc, Callbacks);
end;

function TConversationsRoute.AsyncAwaitAppend(
  const ConversationId: string;
  const ParamProc: TProc<TConversationsParams>;
  const Callbacks: TFunc<TPromiseConversation>): TPromise<TConversation>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TConversation>(
    procedure(const CallbackParams: TFunc<TAsyncConversation>)
    begin
      AsyncAppend(ConversationId, ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TConversationsRoute.AsyncAwaitAppendStream(
  const ConversationId: string; const ParamProc: TProc<TConversationsParams>;
  const Callbacks: TFunc<TPromiseConversationsEvent>): TPromise<string>;
begin
  Result := AsyncAwaitStreamInternal('conversations/' + ConversationId + '#stream', ParamProc, Callbacks);
end;

function TConversationsRoute.AsyncAwaitCreate(
  const ParamProc: TProc<TConversationsParams>;
  const Callbacks: TFunc<TPromiseConversation>): TPromise<TConversation>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TConversation>(
    procedure(const CallbackParams: TFunc<TAsyncConversation>)
    begin
      AsyncCreate(ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TConversationsRoute.AsyncAwaitCreateStream(
  const ParamProc: TProc<TConversationsParams>;
  const Callbacks: TFunc<TPromiseConversationsEvent>): TPromise<string>;
begin
  Result := AsyncAwaitStreamInternal('conversations#stream', ParamProc, Callbacks);
end;

function TConversationsRoute.AsyncAwaitGetHistory(
  const ConversationId: string;
  const Callbacks: TFunc<TPromiseRetrievedEntries>): TPromise<TRetrievedEntries>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TRetrievedEntries>(
    procedure(const CallbackParams: TFunc<TAsyncRetrievedEntries>)
    begin
      AsyncGetHistory(ConversationId, CallbackParams);
    end,
    Callbacks);
end;

function TConversationsRoute.AsyncAwaitGetMessages(
  const ConversationId: string;
  const Callbacks: TFunc<TPromiseRetrieveMessages>): TPromise<TRetrieveMessages>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TRetrieveMessages>(
    procedure(const CallbackParams: TFunc<TAsyncRetrieveMessages>)
    begin
      AsyncGetMessages(ConversationId, CallbackParams);
    end,
    Callbacks);
end;

function TConversationsRoute.AsyncAwaitList(
  const ParamProc: TProc<TConversationsListParams>;
  const Callbacks: TFunc<TPromiseConversationsList>): TPromise<TConversationsList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TConversationsList>(
    procedure(const CallbackParams: TFunc<TAsyncConversationsList>)
    begin
      AsyncList(ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TConversationsRoute.AsyncAwaitCreateParallel(
  const ParamProc: TProc<TBundleParams>;
  const CallBacks: TFunc<TPromiseBundleList>): TPromise<TBundleList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TBundleList>(
    procedure(const CallBackParams: TFunc<TAsynBundleList>)
    begin
      CreateParallel(ParamProc, CallBackParams);
    end,
    CallBacks);
end;

function TConversationsRoute.AsyncAwaitRestart(
  const ConversationId: string;
  const ParamProc: TProc<TConversationsParams>;
  const Callbacks: TFunc<TPromiseConversation>): TPromise<TConversation>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TConversation>(
    procedure(const CallbackParams: TFunc<TAsyncConversation>)
    begin
      AsyncRestart(ConversationId, ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TConversationsRoute.AsyncAwaitRestartStream(
  const ConversationId: string;
  const ParamProc: TProc<TConversationsParams>;
  const Callbacks: TFunc<TPromiseConversationsEvent>): TPromise<string>;
begin
  Result := AsyncAwaitStreamInternal('conversations/' + ConversationId + '/restart#stream', ParamProc, Callbacks);
end;

function TConversationsRoute.AsyncAwaitRetrieve(
  const ConversationId: string;
  const Callbacks: TFunc<TPromiseConversationsListItem>): TPromise<TConversationsListItem>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TConversationsListItem>(
    procedure(const CallbackParams: TFunc<TAsyncConversationsListItem>)
    begin
      AsyncRetrieve(ConversationId, CallbackParams);
    end,
    Callbacks);
end;

procedure TConversationsRoute.AsyncCreate(
  const ParamProc: TProc<TConversationsParams>;
  const Callbacks: TFunc<TAsyncConversation>);
begin
  with TAsyncCallbackExec<TAsyncConversation, TConversation>.Create(Callbacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TConversation
      begin
        Result := Self.Create(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TConversationsRoute.AsyncCreateStream(
  const ParamProc: TProc<TConversationsParams>;
  const Callbacks: TFunc<TAsyncConversationsEvent>);
begin
  AsyncStreamInternal('conversations#stream', ParamProc, Callbacks);
end;

procedure TConversationsRoute.AsyncGetHistory(
  const ConversationId: string;
  const Callbacks: TFunc<TAsyncRetrievedEntries>);
begin
  with TAsyncCallbackExec<TAsyncRetrievedEntries, TRetrievedEntries>.Create(Callbacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TRetrievedEntries
      begin
        Result := Self.GetHistory(ConversationId);
      end);
  finally
    Free;
  end;
end;

procedure TConversationsRoute.AsyncGetMessages(
  const ConversationId: string;
  const Callbacks: TFunc<TAsyncRetrieveMessages>);
begin
  with TAsyncCallbackExec<TAsyncRetrieveMessages, TRetrieveMessages>.Create(Callbacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TRetrieveMessages
      begin
        Result := Self.GetMessages(ConversationId);
      end);
  finally
    Free;
  end;
end;

procedure TConversationsRoute.AsyncList(
  const ParamProc: TProc<TConversationsListParams>;
  const Callbacks: TFunc<TAsyncConversationsList>);
begin
  with TAsyncCallbackExec<TAsyncConversationsList, TConversationsList>.Create(Callbacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TConversationsList
      begin
        Result := Self.List(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TConversationsRoute.AsyncRestart(
  const ConversationId: string;
  const ParamProc: TProc<TConversationsParams>;
  const Callbacks: TFunc<TAsyncConversation>);
begin
  with TAsyncCallbackExec<TAsyncConversation, TConversation>.Create(Callbacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TConversation
      begin
        Result := Self.Restart(ConversationId, ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TConversationsRoute.AsyncRestartStream(
  const ConversationId: string;
  const ParamProc: TProc<TConversationsParams>;
  const Callbacks: TFunc<TAsyncConversationsEvent>);
begin
  AsyncStreamInternal('conversations/' + ConversationId + '/restart#stream', ParamProc, Callbacks);
end;

procedure TConversationsRoute.AsyncRetrieve(
  const ConversationId: string;
  const Callbacks: TFunc<TAsyncConversationsListItem>);
begin
  with TAsyncCallbackExec<TAsyncConversationsListItem, TConversationsListItem>.Create(Callbacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TConversationsListItem
      begin
        Result := Self.Retrieve(ConversationId);
      end);
  finally
    Free;
  end;
end;

function TConversationsRoute.Create(
  const ParamProc: TProc<TConversationsParams>): TConversation;
begin
  Result := API.Post<TConversation, TConversationsParams>('conversations', ParamProc,
     ['outputs', '*', 'content']);
end;

procedure TConversationsRoute.CreateParallel(
  const ParamProc: TProc<TBundleParams>;
  const CallBacks: TFunc<TAsynBundleList>);
var
  Tasks: TArray<ITask>;
  BundleParams: TBundleParams;
begin
  BundleParams := TBundleParams.Create;
  try
    if not Assigned(ParamProc) then
      raise Exception.Create('The lambda can''t be null');

    ParamProc(BundleParams);
    var Bundle := TBundleList.Create;
    var Ranking := 0;
    var ErrorExists := False;
    var Prompts := BundleParams.GetPrompt;
    var Counter := Length(Prompts);

    if Assigned(CallBacks.OnStart) then
      CallBacks.OnStart(CallBacks.Sender);

    SetLength(Tasks, Length(Prompts));
    for var index := 0 to Pred(Length(Prompts)) do
      begin
        Tasks[index] := TTask.Run(
          procedure
          begin
            var Buffer := Bundle.Add(index + 1);
            Buffer.Prompt := Prompts[index];
            try
              var Chat := Create(
                procedure (Params: TConversationsParams)
                begin
                  {--- Set the model for the process }
                  Params.Model(BundleParams.GetModel);

                  {--- Set instructions }
                  Params.Instructions(BundleParams.GetSystem);

                  {--- Set the current prompt and developer message }
                  Params.Inputs(Buffer.Prompt);

                  {--- Don't store the result when parallel execution }
                  Params.Store(False);
                end);
              Inc(Ranking);
              Buffer.FinishIndex := Ranking;
              for var Item in Chat.Outputs do
                for var SubItem in Item.Content do
                  if SubItem.&Type = TContentChunkType.text then
                    Buffer.Response := SubItem.Text;
              Buffer.Chat := Chat;
            except
              on E: Exception do
                begin
                  {--- Catch the exception }
                  var Error := AcquireExceptionObject;
                  ErrorExists := True;
                  try
                    var ErrorMsg := (Error as Exception).Message;
                    {--- Trigger OnError callback if the process has failed }
                    if Assigned(CallBacks.OnError) then
                      TThread.Queue(nil,
                      procedure
                      begin
                        CallBacks.OnError(CallBacks.Sender, ErrorMsg);
                      end);
                  finally
                    {--- Ensures that the instance of the caught exception is released}
                    Error.Free;
                  end;
                end;
            end;
          end);

        if ErrorExists then
          Continue;

        {--- TTask.WaitForAll is not used due to a memory leak in TLightweightEvent/TCompleteEventsWrapper.
             See report RSP-12462 and RSP-25999. }
        TTaskHelper.ContinueWith(Tasks[Index],
          procedure
          begin
            Dec(Counter);
            if Counter = 0 then
              begin
                try
                  if not ErrorExists and Assigned(CallBacks.OnSuccess) then
                    CallBacks.OnSuccess(CallBacks.Sender, Bundle);
                finally
                  Bundle.Free;
                end;
              end;
          end);
        {--- Need a delay, otherwise the process runs only with the first task. }
        Sleep(30);
      end;
  finally
    BundleParams.Free;
  end;
end;

function TConversationsRoute.CreateStream(
  const ParamProc: TProc<TConversationsParams>;
  const Event: TConversationsEventRef): Boolean;
begin
  Result := CreateStreamInternal('conversations#stream', ParamProc, Event);
end;

function TConversationsRoute.GetHistory(
  const ConversationId: string): TRetrievedEntries;
begin
  Result := API.Get<TRetrievedEntries>('conversations/' + ConversationId + '/history',
    ['entries', '*', 'content']);
end;

function TConversationsRoute.GetMessages(
  const ConversationId: string): TRetrieveMessages;
begin
  Result := API.Get<TRetrieveMessages>('conversations/' + ConversationId + '/messages',
    ['messages', '*', 'content']);
end;

function TConversationsRoute.List(
  const ParamProc: TProc<TConversationsListParams>): TConversationsList;
begin
  Result := API.Get<TConversationsList, TConversationsListParams>('conversations', ParamProc, 'data');
end;

function TConversationsRoute.Restart(
  const ConversationId: string;
  const ParamProc: TProc<TConversationsParams>): TConversation;
begin
  Result := API.Post<TConversation, TConversationsParams>('conversations/' + ConversationId + '/restart', ParamProc,
    ['outputs', '*', 'content']);
end;

function TConversationsRoute.RestartStream(
  const ConversationId: string;
  const ParamProc: TProc<TConversationsParams>;
  const Event: TConversationsEventRef): Boolean;
begin
  Result := CreateStreamInternal('conversations/' + ConversationId + '/restart#stream', ParamProc, Event);
end;

function TConversationsRoute.Retrieve(
  const ConversationId: string): TConversationsListItem;
begin
  Result := API.Get<TConversationsListItem>('conversations/' + ConversationId);
end;

end.
