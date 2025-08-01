# Agents & connectors (v1/agents)

- [Introduction](#introduction)
- [Agent creation](#agent-creation)
- [Agent listing](#agent-listing)
- [Get agent](#get-agent)
- [Agent update](#agent-update)
- [Agent version update](#agent-version-update)
- [Connectors](#connectors)
    - [Reasoning](#reasoning)
    - [Web search](#web-search)
    - [Image generation](#image-generation)
    - [Document library](#document-library)
    - [Code interpreter](#code-interpreter)

<br>

___

## Introduction

The API revolves around three primary entities:

- Agent: A predefined configuration that enhances a model’s capabilities—combining tools, instructions, and completion settings to steer behavior.

- Conversation: The stored sequence of interactions, including messages, tool invocations, and other events. Conversations can be initiated either by an Agent or directly by a model.

- Entry: A discrete action produced by a user or assistant. Entries offer a richer, more granular way to represent and control the flow of multi-party interactions and events within a conversation.

Importantly, you don’t need to create an Agent to take advantage of the platform’s functionality. You can interact with the API directly, using built-in conversation features and connectors without wrapping them in an Agent.

For the full specification and all details, consult the [Agents and Conversations](https://docs.mistral.ai/agents/agents_basics/) and [API documentation](https://docs.mistral.ai/api/#tag/beta.agents).

<br>

___

## Agent creation

When you set up a new Agent, you’ll need to configure several key properties up front:

- **model:** Specifies which chat‑completion model the agent will use.
- **description:** A brief summary of the agent’s purpose or the use case it’s intended to handle.
- **name:** The identifier you give your agent.
- **instructions (optional):** The core guidelines or “system” prompt that define the agent’s primary task.
- **tools (optional):** An array of helper utilities the agent can invoke. Available tool types include:
- **function:** Your own custom functions, used much like the standard function‑calling feature in chat completion.
- **web_search / web_search_premium:** Built‑in web‑search capabilities (standard or premium).
- **code_interpreter:** The native code‑execution environment.
- **image_generation:** The integrated image‑creation engine.
- **document_library:** The RAG‑style tool for grounding responses on your own document set.
- **completion_args (optional):** Any standard sampler parameters for chat completions—essentially the same settings you’d pass when making a standalone chat‑completion call.

<br>

___

Create a new agent giving it instructions, tools, description. The agent is then available to be used as a regular assistant in a conversation or as part of an agent pool from which it can be used.

```Delphi
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;

  //Asynchronous promise example
  var Promise := Client.ConversationsAgent.AsyncAwaitCreate(
    procedure (Params: TConversationsAgentParams)
    begin
      Params.Model('mistral-medium-2505');
      Params.Name('just an agent');
      Params.Description('Agent reduced to its simplest form.');
      TutorialHub.JSONRequest := Params.ToFormat();
    end);

  Promise
    .&Then<TConversationsAgent>(
      function (Value: TConversationsAgent): TConversationsAgent
      begin
        Result := Value;
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Asynchronous example
//  Client.ConversationsAgent.AsyncCreate(
//    procedure (Params: TConversationsAgentParams)
//    begin
//      Params.Model('mistral-medium-2505');
//      Params.Name('just an agent');
//      Params.Description('Agent reduced to its simplest form.');
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end,
//    function : TAsyncConversationsAgent
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Value := Client.ConversationsAgent.Create(
//    procedure (Params: TConversationsAgentParams)
//    begin
//      Params.Model('mistral-medium-2505');
//      Params.Name('just an agent');
//      Params.Description('Agent reduced to its simplest form.');
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br>

___

## Agent listing

Retrieve a list of agent entities sorted by creation time.

```Delphi
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;

  //Asynchronous promise example
  var Promise := Client.ConversationsAgent.AsyncAwaitList(
    procedure (Params: TConversationsListParams)
    begin
      Params.Page(0);
      Params.PageSize(100);
    end);

  Promise
    .&Then<TConversationsAgentList>(
      function (Value: TConversationsAgentList): TConversationsAgentList
      begin
        Result := Value;
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Asynchronous example
//  Client.ConversationsAgent.AsyncList(
//    procedure (Params: TConversationsListParams)
//    begin
//      Params.Page(0);
//      Params.PageSize(100);
//    end,
//    function : TAsyncConversationsAgentList
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Value := Client.ConversationsAgent.List(
//    procedure (Params: TConversationsListParams)
//    begin
//      Params.Page(0);
//      Params.PageSize(100);
//    end);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br>

___

## Get agent

Given an agent retrieve an agent entity with its attributes.

```Delphi
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;

  var Agent_id := 'agent_id'; //e.g. 'ag_01985efe4b657539a2d6380dad6c3c38';

  //Asynchronous promise example
  var Promise := Client.ConversationsAgent.AsyncAwaitRetrieve(Agent_id);

  Promise
    .&Then<TConversationsAgent>(
      function (Value: TConversationsAgent): TConversationsAgent
      begin
        Result := Value;
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Asynchronous example
//  Client.ConversationsAgent.AsyncRetrieve(Agent_id,
//    function : TAsyncConversationsAgent
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Value := Client.ConversationsAgent.Retrieve(Agent_id);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br>

___

## Agent update

Update an agent attributes and create a new version.

```Delphi
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;

  var Agent_id := 'agent_id'; //e.g. 'ag_01985efe4b657539a2d6380dad6c3c38';

  //Asynchronous promise example
  var Promise := Client.ConversationsAgent.AsyncAwaitUpdate(Agent_id,
    procedure (Params: TConversationsAgentParams)
    begin
      Params.Model('magistral-medium-2506');
    end);

  Promise
    .&Then<TConversationsAgent>(
      function (Value: TConversationsAgent): TConversationsAgent
      begin
        Result := Value;
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Asynchronous example
//  Client.ConversationsAgent.AsyncUpdate(Agent_id,
//    procedure (Params: TConversationsAgentParams)
//    begin
//      Params.Model('mistral-large-latest');
//    end,
//    function : TAsyncConversationsAgent
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Value := Client.ConversationsAgent.Update(Agent_id,
//    procedure (Params: TConversationsAgentParams)
//    begin
//      Params.Model('magistral-medium-2506');
//    end);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br>

___

## Agent version update

Switch the version of an agent.

```Delphi
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;

  var Agent_id := 'agent_id'; //e.g. 'ag_01985efe4b657539a2d6380dad6c3c38';

  //Asynchronous promise example
  var Promise := Client.ConversationsAgent.AsyncAwaitVersionSwitch(Agent_id,
    procedure (Params: TAgentVersionParams)
    begin
      Params.Version(4);
    end);

  Promise
    .&Then<TConversationsAgent>(
      function (Value: TConversationsAgent): TConversationsAgent
      begin
        Result := Value;
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Asynchronous example
//  Client.ConversationsAgent.AsyncVersionSwitch(Agent_id,
//    procedure (Params: TAgentVersionParams)
//    begin
//      Params.Version(3);
//    end,
//    function : TAsyncConversationsAgent
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Value := Client.ConversationsAgent.VersionSwitch(Agent_id,
//    procedure (Params: TAgentVersionParams)
//    begin
//      Params.Version(1);
//    end);
//  try
//    Display(TutorialHub, Value.Name);
//  finally
//    Value.Free;
//  end;
```

<br>

___

## Connectors

Only code snippets for the asynchronous approach (returning a Promise) are included. Templates for synchronous implementations and straightforward asynchronous cases have already been provided; adapting them takes only seconds and is left to the reader.

- [Reasoning](#reasoning)
- [Web search](#web-search)
- [Image generation](#image-generation)
- [Document library](#document-library)
- [Code interpreter](#code-interpreter)

<br>

___

### Reasoning

#### Agent creation

```Delphi
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;

  //Asynchronous promise example
  var Promise := Client.ConversationsAgent.AsyncAwaitCreate(
    procedure (Params: TConversationsAgentParams)
    begin
      Params.Model('magistral-medium-2506');
      Params.Name('Web Search Agent');
      Params.Description('Agent able to reasoning');
      Params.Instructions; //use the string `ReasoningEnglishInstructions` defined in the MistralAI.Types unit.
      Params.Tools([web_search]);
      Params.CompletionArgs(
        TCompletionArgsParams.Create
          .Temperature(0.3)
          .TopP(0.95)
        );
      TutorialHub.JSONRequest := Params.ToFormat();
    end);

  Promise
    .&Then<TConversationsAgent>(
      function (Value: TConversationsAgent): TConversationsAgent
      begin
        Result := Value;
        Display(TutorialHub, 'Agent created');
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
```

<br>

___

#### Using the agent

```Delphi
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;

  var Agent_id := 'id_value';  //e.g. ag_01985efe4b657539a2d6380dad6c5c66

  //Asynchronous promise example
  var Promise := Client.Conversations.AsyncAwaitCreateStream(
    procedure (Params: TConversationsParams)
    begin
      Params.Inputs('John is one of 4 children. The first sister is 4 years old. Next year, the second sister will be twice as old as the first sister. The third sister is two years older than the second sister. The third sister is half the age of her older brother. How old is John?');
      Params.Stream;
      Params.AgentId(Agent_id);
      Params.Store(False);
      TutorialHub.JSONRequest := Params.ToFormat();
    end,
    function : TPromiseConversationsEvent
    begin
      Result.Sender := TutorialHub;
      Result.OnProgress :=
        procedure (Sender: TObject; Event: TConversationsEvent)
        begin
          DisplayStream(Sender, Event);
        end;
      Result.OnDoCancel := DoCancellation;
      Result.OnCancellation :=
        function (Sender: TObject): string
        begin
          Cancellation(Sender);
        end;
    end);

  Promise
    .&Then<string>(
      function (Value: string): string
      begin
        Result := Value;
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);  
```

<br>

___

### Web search

#### Agent creation

```Delphi
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;
  
  TutorialHub.JSONRequestClear;

  //Asynchronous promise example
  var Promise := Client.ConversationsAgent.AsyncAwaitCreate(
    procedure (Params: TConversationsAgentParams)
    begin
      Params.Model('mistral-medium-2505');
      Params.Name('Websearch Agent');
      Params.Description('Agent able to search information over the web, such as news, weather, sport results...');
      Params.Instructions('You have the ability to perform web searches with `web_search` to find up-to-date information.');
      Params.Tools([web_search]); // or Params.Tools([web_search_premium]);
      Params.CompletionArgs(
        TCompletionArgsParams.Create
          .Temperature(0.3)
          .TopP(0.95)
        );
      TutorialHub.JSONRequest := Params.ToFormat();
    end);

  Promise
    .&Then<TConversationsAgent>(
      function (Value: TConversationsAgent): TConversationsAgent
      begin
        Result := Value;
        Display(TutorialHub, 'Agent created');
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
```

<br>

___

#### Using the agent

```Delphi
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;

  var Agent_id := 'id_value';  //e.g. ag_01985efe4b657539a2d6380dad6c5c66

  //Asynchronous promise example
  Start(TutorialHub);
  var Promise := Client.Conversations.AsyncAwaitCreate(
    procedure (Params: TConversationsParams)
    begin
      Params
        .Inputs('Donne-moi les nouvelles du jour pour la France.')
        .AgentId(Agent_id)
        .Store(False);
      TutorialHub.JSONRequest := Params.ToFormat();
    end);

  Promise
    .&Then<TConversation>(
      function (Value: TConversation): TConversation
      begin
        Result := Value;
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
end;
```

<br>

___

### Image generation

#### Agent creation

```Delphi
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;

  //Asynchronous promise example
  var Promise := Client.ConversationsAgent.AsyncAwaitCreate(
    procedure (Params: TConversationsAgentParams)
    begin
      Params.Model('mistral-medium-2505');
      Params.Name('Image generation Agent');
      Params.Description('Agent used to generate images.');
      Params.Instructions('Use the image generation tool when you have to create images.');
      Params.Tools([image_generation]);
      Params.CompletionArgs(
        TCompletionArgsParams.Create
          .Temperature(0.3)
          .TopP(0.95)
        );
      TutorialHub.JSONRequest := Params.ToFormat();
    end);

  Promise
    .&Then<TConversationsAgent>(
      function (Value: TConversationsAgent): TConversationsAgent
      begin
        Result := Value;
        Display(TutorialHub, 'Agent created');
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
```

<br>

___

#### Using the agent

```Delphi
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;

  var Agent_id := 'id_value';  //e.g. ag_01985efe4b657539a2d6380dad6c5c66

  //Asynchronous promise example
  Start(TutorialHub);
  var Promise := Client.Conversations.AsyncAwaitCreate(
    procedure (Params: TConversationsParams)
    begin
      Params
        .Inputs('Generate an image of a neutron star interacting directly with a black hole')
        .AgentId(Agent_id)
        .Store(False);
      TutorialHub.JSONRequest := Params.ToFormat();
    end);

  Promise
    .&Then<TConversation>(
      function (Value: TConversation): TConversation
      begin
        Result := Value;
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
```

<br>

___

### Document library

#### Agent creation

```Delphi
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  //Asynchronous promise example
  var Promise := Client.ConversationsAgent.AsyncAwaitCreate(
    procedure (Params: TConversationsAgentParams)
    begin
      Params.Model('mistral-medium-2505');
      Params.Name('Library Agent');
      Params.Instructions('You have the ability to perform searches with `document_library` to find relevant information.');
      Params.Description('Agent able to search information in your library to answer all questions regarding the Raoul project');
      Params.Tools([document_library([library_id])]); //e.g. library_id = '0198502a-6f55-578e-9976-570167da0f14'
      Params.CompletionArgs(
        TCompletionArgsParams.Create
          .Temperature(0.3)
          .TopP(0.95)
        );
      TutorialHub.JSONRequest := Params.ToFormat();
    end);

  Promise
    .&Then<TConversationsAgent>(
      function (Value: TConversationsAgent): TConversationsAgent
      begin
        Result := Value;
        Display(TutorialHub, 'Agent created');
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
```

<br>

___

#### Using the agent

```Delphi
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

    TutorialHub.JSONRequestClear;

  var Agent_id := 'id_value';  //e.g. ag_01985efe4b657539a2d6380dad6c5c66

  //Asynchronous promise example
  var Promise := Client.Conversations.AsyncAwaitCreateStream(
     procedure (Params: TConversationsParams)
    begin
      Params
        .Inputs(Memo2.Text)
        .AgentId(Agent_id)
        .Stream
        .Store;
      TutorialHub.JSONRequest := Params.ToFormat();
    end,
    function : TPromiseConversationsEvent
    begin
      Result.Sender := TutorialHub;
      Result.OnProgress :=
        procedure (Sender: TObject; Event: TConversationsEvent)
        begin
          if Event.&Type = TChunkEvent.conversation_response_started then
            ConvId := Event.ConversationId;
          DisplayStream(Sender, Event);
        end;
      Result.OnDoCancel := DoCancellation;
      Result.OnCancellation :=
        function (Sender: TObject): string
        begin
          Cancellation(Sender);
        end;
    end);

  Promise
    .&Then<string>(
      function (Value: string): string
      begin
        Result := Value;
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
```

<br>

___

### Code interpreter

#### Agent creation

```Delphi
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  //Asynchronous promise example
  var Promise := Client.ConversationsAgent.AsyncAwaitCreate(
    procedure (Params: TConversationsAgentParams)
    begin
      Params.Model('mistral-medium-2505');
      Params.Name('Coding Agent');
      Params.Description('Agent used to execute code using the interpreter tool.');
      Params.Instructions('Use the code interpreter tool when you have to run code.');
      Params.Tools([code_interpreter]);
      Params.CompletionArgs(
        TCompletionArgsParams.Create
          .Temperature(0.3)
          .TopP(0.95)
        );
      TutorialHub.JSONRequest := Params.ToFormat();
    end);

  Promise
    .&Then<TConversationsAgent>(
      function (Value: TConversationsAgent): TConversationsAgent
      begin
        Result := Value;
        Display(TutorialHub, 'Agent created');
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
```

<br>

___

#### Using the agent

```Delphi
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;

  var Agent_id := 'id_value';  //e.g. ag_01985efe4b657539a2d6380dad6c5c66

  //Asynchronous promise example
  Start(TutorialHub);
  var Promise := Client.Conversations.AsyncAwaitCreate(
    procedure (Params: TConversationsParams)
    begin
      Params
        .Inputs('Run a fibonacci function for the first 20 values.')
        .AgentId(Agent_id)
        .Store;
      TutorialHub.JSONRequest := Params.ToFormat();
    end);

  Promise
    .&Then<TConversation>(
      function (Value: TConversation): TConversation
      begin
        Result := Value;
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
```

