unit MistralAI.Conversations.Agents;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiMistralAI
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes,
  MistralAI.API.Params, MistralAI.API, MistralAI.Types, MistralAI.Conversations.Params,
  MistralAI.Async.Params, MistralAI.Async.Support, MistralAI.Async.Promise,
  MistralAI.Conversations.Chunks, MistralAI.Conversations.Manager;

type
  /// <summary>
  /// Defines an asynchronous callback type for operations returning a <see cref="TConversationsAgent"/> instance.
  /// </summary>
  /// <remarks>
  /// <para>
  /// <c>TAsyncConversationsAgent</c> is an alias for <c>TAsyncCallback&lt;TConversationsAgent&gt;</c>,
  /// providing a structured mechanism for handling the lifecycle of asynchronous agent creation or retrieval
  /// calls. Use this callback to register <c>OnStart</c>, <c>OnSuccess</c>, and <c>OnError</c> handlers
  /// when interacting with the <c>TConversationsAgentRoute</c>.
  /// </para>
  /// <para>
  /// The <c>TConversationsAgent</c> returned encapsulates metadata and state associated with a conversation
  /// agent, including its configuration, description, and available tools.
  /// </para>
  /// </remarks>
  TAsyncConversationsAgent = TAsyncCallback<TConversationsAgent>;

  /// <summary>
  /// Defines a promise-based asynchronous callback for operations that return a <see cref="TConversationsAgent"/>.
  /// </summary>
  /// <remarks>
  /// <para>
  /// <c>TPromiseConversationsAgent</c> is an alias for <c>TPromiseCallback&lt;TConversationsAgent&gt;</c>,
  /// providing a task-like interface for creating or retrieving conversation agents asynchronously.
  /// </para>
  /// <para>
  /// Use this promise to await the result of an agent operation—resolving with a <c>TConversationsAgent</c>
  /// instance on success or raising an exception on error.
  /// </para>
  /// </remarks>
  TPromiseConversationsAgent = TPromiseCallback<TConversationsAgent>;

  /// <summary>
  /// Defines an asynchronous callback type for operations returning a <see cref="TConversationsAgentList"/> instance.
  /// </summary>
  /// <remarks>
  /// <para>
  /// <c>TAsyncConversationsAgentList</c> is an alias for <c>TAsyncCallback&lt;TConversationsAgentList&gt;</c>,
  /// providing a structured mechanism for handling the lifecycle of asynchronous calls that list conversation agents.
  /// </para>
  /// <para>
  /// Use this callback to register <c>OnStart</c>, <c>OnSuccess</c>, and <c>OnError</c> handlers when invoking
  /// <c>TConversationsAgentRoute.List</c> or its asynchronous variants.
  /// </para>
  /// <para>
  /// The <c>TConversationsAgentList</c> returned contains metadata and a collection of <c>TConversationsAgent</c>
  /// entries, representing the available agents.
  /// </para>
  /// </remarks>
  TAsyncConversationsAgentList = TAsyncCallback<TConversationsAgentList>;

  /// <summary>
  /// Defines a promise-based asynchronous callback for operations returning a <see cref="TConversationsAgentList"/>.
  /// </summary>
  /// <remarks>
  /// <para>
  /// <c>TPromiseConversationsAgentList</c> is an alias for <c>TPromiseCallback&lt;TConversationsAgentList&gt;</c>,
  /// providing a task-like interface for retrieving a list of conversation agents asynchronously.
  /// </para>
  /// <para>
  /// Use this promise to await the result of a list operation—resolving with a <c>TConversationsAgentList</c>
  /// instance on success or raising an exception on failure.
  /// </para>
  /// </remarks>
  TPromiseConversationsAgentList = TPromiseCallback<TConversationsAgentList>;

  /// <summary>
  /// Provides access to the Conversations Agents API, enabling creation, retrieval, listing, updating, and version management of conversation agents.
  /// </summary>
  /// <remarks>
  /// <para>
  /// <c>TConversationsAgentRoute</c> extends <see cref="TMistralAIAPIRoute"/> to encapsulate HTTP interactions
  /// with the `/agents` endpoints. It offers both synchronous and asynchronous methods for agent lifecycle management.
  /// </para>
  /// <para>
  /// Synchronous operations include <see cref="Create"/>, <see cref="List"/>, <see cref="Retrieve"/>,
  /// <see cref="Update"/>, and <see cref="VersionSwitch"/>. For non-blocking workflows, use the
  /// asynchronous callbacks <see cref="AsyncCreate"/>, <see cref="AsyncList"/>, <see cref="AsyncRetrieve"/>,
  /// <see cref="AsyncUpdate"/>, and <see cref="AsyncVersionSwitch"/>, or their promise-based counterparts
  /// <see cref="AsyncAwaitCreate"/>, <see cref="AsyncAwaitList"/>, <see cref="AsyncAwaitRetrieve"/>,
  /// <see cref="AsyncAwaitUpdate"/>, and <see cref="AsyncAwaitVersionSwitch"/>.
  /// </para>
  /// </remarks>
  TConversationsAgentRoute = class(TMistralAIAPIRoute)

    /// <summary>
    /// Initiates an asynchronous creation of a conversation agent and returns a promise for the result.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to configure the <see cref="TConversationsAgentParams"/> for the request, such as model, name, description, tools, and completion arguments.
    /// </param>
    /// <param name="Callbacks">
    /// An optional promise-based callback (<see cref="TPromiseConversationsAgent"/>) to handle lifecycle events:
    /// resolution with a <see cref="TConversationsAgent"/> on success or exception on error.
    /// </param>
    /// <returns>
    /// A <see cref="TPromise{TConversationsAgent}"/> that resolves to the created <see cref="TConversationsAgent"/>.
    /// </returns>
    /// <remarks>
    /// This method wraps the standard <see cref="AsyncCreate"/> callback pattern into a promise-based interface.
    /// Use <c>await</c> on the returned promise to obtain the <see cref="TConversationsAgent"/> once the API call completes.
    /// If <paramref name="Callbacks"/> is provided, its handlers will be invoked during the request lifecycle.
    /// </remarks>
    function AsyncAwaitCreate(
      const ParamProc: TProc<TConversationsAgentParams>;
      const Callbacks: TFunc<TPromiseConversationsAgent> = nil): TPromise<TConversationsAgent>;

    /// <summary>
    /// Initiates an asynchronous request to retrieve the list of conversation agents and returns a promise for the result.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to configure the <see cref="TConversationsListParams"/>, such as filtering, pagination, or sorting options.
    /// </param>
    /// <param name="Callbacks">
    /// An optional promise-based callback (<see cref="TPromiseConversationsAgentList"/>) to handle lifecycle events:
    /// resolution with a <see cref="TConversationsAgentList"/> on success or exception on failure.
    /// </param>
    /// <returns>
    /// A <see cref="TPromise{TConversationsAgentList}"/> that resolves to the retrieved list of <see cref="TConversationsAgent"/> instances.
    /// </returns>
    /// <remarks>
    /// This method wraps the <see cref="AsyncList"/> callback pattern into a promise-based interface.
    /// Use <c>await</c> on the returned promise to obtain the <see cref="TConversationsAgentList"/> once the API call completes.
    /// If <paramref name="Callbacks"/> is provided, its handlers will be invoked during the request lifecycle.
    /// </remarks>
    function AsyncAwaitList(
      const ParamProc: TProc<TConversationsListParams>;
      const Callbacks: TFunc<TPromiseConversationsAgentList> = nil): TPromise<TConversationsAgentList>;

    /// <summary>
    /// Initiates an asynchronous request to retrieve a specific conversation agent by its identifier and returns a promise for the result.
    /// </summary>
    /// <param name="AgentId">
    /// The unique identifier of the conversation agent to retrieve.
    /// </param>
    /// <param name="Callbacks">
    /// An optional promise-based callback (<see cref="TPromiseConversationsAgent"/>) to handle lifecycle events:
    /// resolution with a <see cref="TConversationsAgent"/> instance on success or exception on error.
    /// </param>
    /// <returns>
    /// A <see cref="TPromise{TConversationsAgent}"/> that resolves to the requested <see cref="TConversationsAgent"/>.
    /// </returns>
    /// <remarks>
    /// This method wraps the <see cref="AsyncRetrieve"/> callback pattern into a promise-based interface.
    /// Use <c>await</c> on the returned promise to obtain the <see cref="TConversationsAgent"/> once the API call completes.
    /// If <paramref name="Callbacks"/> is provided, its handlers will be invoked during the request lifecycle.
    /// </remarks>
    function AsyncAwaitRetrieve(
      const AgentId: string;
      const Callbacks: TFunc<TPromiseConversationsAgent> = nil): TPromise<TConversationsAgent>;

    /// <summary>
    /// Initiates an asynchronous update of a conversation agent’s settings and returns a promise for the updated agent.
    /// </summary>
    /// <param name="AgentId">
    /// The unique identifier of the conversation agent to update.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure to configure the <see cref="TConversationsAgentParams"/>, such as modifying tools, description, or completion arguments.
    /// </param>
    /// <param name="Callbacks">
    /// An optional promise-based callback (<see cref="TPromiseConversationsAgent"/>) to handle lifecycle events:
    /// resolution with the updated <see cref="TConversationsAgent"/> on success or exception on error.
    /// </param>
    /// <returns>
    /// A <see cref="TPromise{TConversationsAgent}"/> that resolves to the updated <see cref="TConversationsAgent"/>.
    /// </returns>
    /// <remarks>
    /// This method wraps the <see cref="AsyncUpdate"/> callback pattern into a promise-based interface.
    /// Use <c>await</c> on the returned promise to receive the updated agent once the API call completes.
    /// If <paramref name="Callbacks"/> is provided, its handlers will be invoked during the request lifecycle.
    /// </remarks>
    function AsyncAwaitUpdate(
      const AgentId: string;
      const ParamProc: TProc<TConversationsAgentParams>;
      const Callbacks: TFunc<TPromiseConversationsAgent> = nil): TPromise<TConversationsAgent>;

    /// <summary>
    /// Initiates an asynchronous version switch for a conversation agent and returns a promise for the resulting agent.
    /// </summary>
    /// <param name="AgentId">
    /// The unique identifier of the conversation agent whose version will be switched.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure to configure the <see cref="TAgentVersionParams"/>, specifying the target version number.
    /// </param>
    /// <param name="Callbacks">
    /// An optional promise-based callback (<see cref="TPromiseConversationsAgent"/>) to handle lifecycle events:
    /// resolution with the <see cref="TConversationsAgent"/> of the new version on success or exception on error.
    /// </param>
    /// <returns>
    /// A <see cref="TPromise{TConversationsAgent}"/> that resolves to the <see cref="TConversationsAgent"/> switched to the specified version.
    /// </returns>
    /// <remarks>
    /// This method wraps the <see cref="VersionSwitch"/> callback into a promise-based interface.
    /// Use <c>await</c> on the returned promise to obtain the agent at its new version once the API call completes.
    /// If <paramref name="Callbacks"/> is provided, its handlers will be invoked during the request lifecycle.
    /// </remarks>
    function AsyncAwaitVersionSwitch(const AgentId: string;
      const ParamProc: TProc<TAgentVersionParams>;
      const Callbacks: TFunc<TPromiseConversationsAgent> = nil): TPromise<TConversationsAgent>;

    /// <summary>
    /// Creates a new conversation agent synchronously using the specified parameters.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to configure the <see cref="TConversationsAgentParams"/>, including model selection, name, description, tools, and completion arguments.
    /// </param>
    /// <returns>
    /// A <see cref="TConversationsAgent"/> instance representing the newly created agent.
    /// </returns>
    /// <remarks>
    /// This method performs a blocking HTTP POST to the /agents endpoint. Use it when you need to create
    /// an agent and immediately work with the returned object. For non-blocking workflows, consider using
    /// <see cref="AsyncCreate"/> or <see cref="AsyncAwaitCreate"/>.
    /// </remarks>
    function Create(const ParamProc: TProc<TConversationsAgentParams>): TConversationsAgent;

    /// <summary>
    /// Retrieves a list of conversation agents synchronously using the specified parameters.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to configure the <see cref="TConversationsListParams"/>, such as filtering, pagination, or sorting options.
    /// </param>
    /// <returns>
    /// A <see cref="TConversationsAgentList"/> instance containing the retrieved collection of agents.
    /// </returns>
    /// <remarks>
    /// This method performs a blocking HTTP GET to the /agents endpoint. Use it when you need to fetch
    /// agents immediately. For non-blocking workflows, consider using <see cref="AsyncList"/> or <see cref="AsyncAwaitList"/>.
    /// </remarks>
    function List(const ParamProc: TProc<TConversationsListParams>): TConversationsAgentList;

    /// <summary>
    /// Retrieves a specific conversation agent synchronously by its identifier.
    /// </summary>
    /// <param name="AgentId">
    /// The unique identifier of the conversation agent to retrieve.
    /// </param>
    /// <returns>
    /// A <see cref="TConversationsAgent"/> instance representing the requested agent.
    /// </returns>
    /// <remarks>
    /// This method performs a blocking HTTP GET to the /agents/{AgentId} endpoint.
    /// Use it when you need to fetch an agent’s details immediately.
    /// For non-blocking workflows, consider using <see cref="AsyncRetrieve"/> or <see cref="AsyncAwaitRetrieve"/>.
    /// </remarks>
    function Retrieve(const AgentId: string): TConversationsAgent;

    /// <summary>
    /// Updates an existing conversation agent synchronously with the specified parameters.
    /// </summary>
    /// <param name="AgentId">
    /// The unique identifier of the conversation agent to update.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure to configure the <see cref="TConversationsAgentParams"/>, such as modifying the agent’s name, description, tools, or completion arguments.
    /// </param>
    /// <returns>
    /// A <see cref="TConversationsAgent"/> instance representing the updated agent.
    /// </returns>
    /// <remarks>
    /// This method performs a blocking HTTP PATCH to the /agents/{AgentId} endpoint. Use it when you
    /// need to apply configuration changes to an agent and work with the updated object immediately.
    /// For non-blocking workflows, consider using <see cref="AsyncUpdate"/> or <see cref="AsyncAwaitUpdate"/>.
    /// </remarks>
    function Update(const AgentId: string;
      const ParamProc: TProc<TConversationsAgentParams>): TConversationsAgent;

    /// <summary>
    /// Switches the active version of a conversation agent synchronously.
    /// </summary>
    /// <param name="AgentId">
    /// The unique identifier of the conversation agent whose version will be switched.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure to configure the <see cref="TAgentVersionParams"/>, specifying the target version number.
    /// </param>
    /// <returns>
    /// A <see cref="TConversationsAgent"/> instance representing the agent at the newly selected version.
    /// </returns>
    /// <remarks>
    /// This method performs a blocking HTTP PATCH to the /agents/{AgentId}/version endpoint.
    /// Use it when you need to immediately switch an agent to a different version and work with the updated configuration.
    /// For non-blocking workflows, consider using <see cref="AsyncAwaitVersionSwitch"/> or <see cref="AsyncAwaitVersionSwitch"/>.
    /// </remarks>
    function VersionSwitch(const AgentId: string;
      const ParamProc: TProc<TAgentVersionParams>): TConversationsAgent;

    /// <summary>
    /// Initiates an asynchronous creation of a conversation agent using callbacks.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to configure the <see cref="TConversationsAgentParams"/>, including model selection, name, description, tools, and completion arguments.
    /// </param>
    /// <param name="Callbacks">
    /// A callback delegate (<see cref="TAsyncConversationsAgent"/>) to handle lifecycle events:
    /// <c>OnStart</c> when the request begins, <c>OnSuccess</c> with the created <see cref="TConversationsAgent"/>, and <c>OnError</c> on failure.
    /// </param>
    /// <remarks>
    /// This method performs a non-blocking HTTP POST to the /agents endpoint.
    /// Use it when you need to create an agent without blocking the calling thread.
    /// For promise-based flows, see <see cref="AsyncAwaitCreate"/>.
    /// </remarks>
    procedure AsyncCreate(
      const ParamProc: TProc<TConversationsAgentParams>;
      const Callbacks: TFunc<TAsyncConversationsAgent>);

    /// <summary>
    /// Initiates an asynchronous request to list conversation agents using callbacks.
    /// </summary>
    /// <param name="ParamProc">
    /// A procedure to configure the <see cref="TConversationsListParams"/>, such as filtering, pagination, or sorting options.
    /// </param>
    /// <param name="Callbacks">
    /// A callback delegate (<see cref="TAsyncConversationsAgentList"/>) to handle lifecycle events:
    /// <c>OnStart</c> when the request begins, <c>OnSuccess</c> with the resulting <see cref="TConversationsAgentList"/>, and <c>OnError</c> on failure.
    /// </param>
    /// <remarks>
    /// This method performs a non-blocking HTTP GET to the /agents endpoint.
    /// Use it when you need to retrieve the list of agents without blocking the calling thread.
    /// For promise-based flows, see <see cref="AsyncAwaitList"/>.
    /// </remarks>
    procedure AsyncList(
      const ParamProc: TProc<TConversationsListParams>;
      const Callbacks: TFunc<TAsyncConversationsAgentList>);

    /// <summary>
    /// Initiates an asynchronous retrieval of a specific conversation agent using callbacks.
    /// </summary>
    /// <param name="AgentId">
    /// The unique identifier of the conversation agent to retrieve.
    /// </param>
    /// <param name="Callbacks">
    /// A callback delegate (<see cref="TAsyncConversationsAgent"/>) to handle lifecycle events:
    /// <c>OnStart</c> when the request begins, <c>OnSuccess</c> with the retrieved <see cref="TConversationsAgent"/>, and <c>OnError</c> on failure.
    /// </param>
    /// <remarks>
    /// This method performs a non-blocking HTTP GET to the /agents/{AgentId} endpoint.
    /// Use it when you need to fetch an agent’s details without blocking the calling thread.
    /// For promise-based workflows, see <see cref="AsyncAwaitRetrieve"/>.
    /// </remarks>
    procedure AsyncRetrieve(
      const AgentId: string;
      const Callbacks: TFunc<TAsyncConversationsAgent>);

    /// <summary>
    /// Initiates an asynchronous update of a conversation agent using callbacks.
    /// </summary>
    /// <param name="AgentId">
    /// The unique identifier of the conversation agent to update.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure to configure the <see cref="TConversationsAgentParams"/>, such as modifying tools, description, or completion arguments.
    /// </param>
    /// <param name="Callbacks">
    /// A callback delegate (<see cref="TAsyncConversationsAgent"/>) to handle lifecycle events:
    /// <c>OnStart</c> when the request begins, <c>OnSuccess</c> with the updated <see cref="TConversationsAgent"/>, and <c>OnError</c> on failure.
    /// </param>
    /// <remarks>
    /// This method performs a non-blocking HTTP PATCH to the /agents/{AgentId} endpoint.
    /// Use it when you need to apply configuration changes without blocking the calling thread.
    /// For promise-based flows, see <see cref="AsyncAwaitUpdate"/>.
    /// </remarks>
    procedure AsyncUpdate(
      const AgentId: string;
      const ParamProc: TProc<TConversationsAgentParams>;
      const Callbacks: TFunc<TAsyncConversationsAgent>);

    /// <summary>
    /// Initiates an asynchronous version switch of a conversation agent using callbacks.
    /// </summary>
    /// <param name="AgentId">
    /// The unique identifier of the conversation agent whose version will be switched.
    /// </param>
    /// <param name="ParamProc">
    /// A procedure to configure the <see cref="TAgentVersionParams"/>, specifying the target version number.
    /// </param>
    /// <param name="Callbacks">
    /// A callback delegate (<see cref="TAsyncConversationsAgent"/>) to handle lifecycle events:
    /// <c>OnStart</c> when the request begins, <c>OnSuccess</c> with the <see cref="TConversationsAgent"/> of the new version, and <c>OnError</c> on failure.
    /// </param>
    /// <remarks>
    /// This method performs a non-blocking HTTP PATCH to the /agents/{AgentId}/version endpoint.
    /// Use it when you need to switch an agent’s version without blocking the calling thread.
    /// For promise-based workflows, see <see cref="AsyncAwaitVersionSwitch"/>.
    /// </remarks>
    procedure AsyncVersionSwitch(
      const AgentId: string;
      const ParamProc: TProc<TAgentVersionParams>;
      const Callbacks: TFunc<TAsyncConversationsAgent>);
  end;

implementation

{ TConversationsAgentRoute }

function TConversationsAgentRoute.AsyncAwaitCreate(
  const ParamProc: TProc<TConversationsAgentParams>;
  const Callbacks: TFunc<TPromiseConversationsAgent>): TPromise<TConversationsAgent>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TConversationsAgent>(
    procedure(const CallbackParams: TFunc<TAsyncConversationsAgent>)
    begin
      AsyncCreate(ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TConversationsAgentRoute.AsyncAwaitList(
  const ParamProc: TProc<TConversationsListParams>;
  const Callbacks: TFunc<TPromiseConversationsAgentList>): TPromise<TConversationsAgentList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TConversationsAgentList>(
    procedure(const CallbackParams: TFunc<TAsyncConversationsAgentList>)
    begin
      AsyncList(ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TConversationsAgentRoute.AsyncAwaitRetrieve(const AgentId: string;
  const Callbacks: TFunc<TPromiseConversationsAgent>): TPromise<TConversationsAgent>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TConversationsAgent>(
    procedure(const CallbackParams: TFunc<TAsyncConversationsAgent>)
    begin
      AsyncRetrieve(AgentId, CallbackParams);
    end,
    Callbacks);
end;

function TConversationsAgentRoute.AsyncAwaitUpdate(const AgentId: string;
  const ParamProc: TProc<TConversationsAgentParams>;
  const Callbacks: TFunc<TPromiseConversationsAgent>): TPromise<TConversationsAgent>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TConversationsAgent>(
    procedure(const CallbackParams: TFunc<TAsyncConversationsAgent>)
    begin
      AsyncUpdate(AgentId, ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TConversationsAgentRoute.AsyncAwaitVersionSwitch(const AgentId: string;
  const ParamProc: TProc<TAgentVersionParams>;
  const Callbacks: TFunc<TPromiseConversationsAgent>): TPromise<TConversationsAgent>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TConversationsAgent>(
    procedure(const CallbackParams: TFunc<TAsyncConversationsAgent>)
    begin
      AsyncVersionSwitch(AgentId, ParamProc, CallbackParams);
    end,
    Callbacks);
end;

procedure TConversationsAgentRoute.AsyncCreate(
  const ParamProc: TProc<TConversationsAgentParams>;
  const Callbacks: TFunc<TAsyncConversationsAgent>);
begin
  with TAsyncCallbackExec<TAsyncConversationsAgent, TConversationsAgent>.Create(Callbacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TConversationsAgent
      begin
        Result := Self.Create(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TConversationsAgentRoute.AsyncList(
  const ParamProc: TProc<TConversationsListParams>;
  const Callbacks: TFunc<TAsyncConversationsAgentList>);
begin
  with TAsyncCallbackExec<TAsyncConversationsAgentList, TConversationsAgentList>.Create(Callbacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TConversationsAgentList
      begin
        Result := Self.List(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TConversationsAgentRoute.AsyncRetrieve(const AgentId: string;
  const Callbacks: TFunc<TAsyncConversationsAgent>);
begin
  with TAsyncCallbackExec<TAsyncConversationsAgent, TConversationsAgent>.Create(Callbacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TConversationsAgent
      begin
        Result := Self.Retrieve(AgentId);
      end);
  finally
    Free;
  end;
end;

procedure TConversationsAgentRoute.AsyncUpdate(const AgentId: string;
  const ParamProc: TProc<TConversationsAgentParams>;
  const Callbacks: TFunc<TAsyncConversationsAgent>);
begin
  with TAsyncCallbackExec<TAsyncConversationsAgent, TConversationsAgent>.Create(Callbacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TConversationsAgent
      begin
        Result := Self.Update(AgentId, ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TConversationsAgentRoute.AsyncVersionSwitch(const AgentId: string;
  const ParamProc: TProc<TAgentVersionParams>;
  const Callbacks: TFunc<TAsyncConversationsAgent>);
begin
  with TAsyncCallbackExec<TAsyncConversationsAgent, TConversationsAgent>.Create(Callbacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TConversationsAgent
      begin
        Result := Self.VersionSwitch(AgentId, ParamProc);
      end);
  finally
    Free;
  end;
end;

function TConversationsAgentRoute.Create(
  const ParamProc: TProc<TConversationsAgentParams>): TConversationsAgent;
begin
  Result := API.Post<TConversationsAgent, TConversationsAgentParams>('agents', ParamProc);
end;

function TConversationsAgentRoute.List(
  const ParamProc: TProc<TConversationsListParams>): TConversationsAgentList;
begin
  Result := API.Get<TConversationsAgentList, TConversationsListParams>('agents', ParamProc, 'data');
end;

function TConversationsAgentRoute.Retrieve(
  const AgentId: string): TConversationsAgent;
begin
  Result := API.Get<TConversationsAgent>('agents/' + AgentId);
end;

function TConversationsAgentRoute.Update(const AgentId: string;
  const ParamProc: TProc<TConversationsAgentParams>): TConversationsAgent;
begin
  Result := API.Patch<TConversationsAgent, TConversationsAgentParams>('agents/' + AgentId, ParamProc);
end;

function TConversationsAgentRoute.VersionSwitch(const AgentId: string;
  const ParamProc: TProc<TAgentVersionParams>): TConversationsAgent;
begin
  Result := API.PatchFromUrl<TConversationsAgent, TAgentVersionParams>('agents/' + AgentId + '/version', ParamProc);
end;

end.
