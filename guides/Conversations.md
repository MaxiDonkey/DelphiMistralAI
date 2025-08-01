# Conversations API (v1/conversations)

- [Introduction](#introduction)
- [Text generation](#text-generation)
    - [Non streamed](#non-streamed)
    - [Streamed](#streamed)
    - [Multi-turn conversations](#multi-turn-conversations) 
    - [Parallel method for generating text](#parallel-method-for-generating-text)
    - [Reasoning](#reasoning)
- [Conversations managment](#conversations-managment)
    - [Continue a conversation](#continue-a-conversation)
    - [Retrieve conversations](#retrieve-conversations)
    - [Restart a conversation](#restart-a-conversation)
    - [List all created conversations](#list-all-created-conversations)
    - [Retrieve all entries in a conversation](#retrieve-all-entries-in-a-conversation)
    - [Retrieve all messages in a conversation](#retrieve-all-messages-in-a-conversation)
- [Tools for conversations](#tools-for-conversations)
- [Function calling](#function-calling)
- [Structured Output](#structured-output)
- [Agents & connectors](#agents--connectors)

<br>

___

## Introduction

Once your agent is created, you can start or resume a conversation at any time using the same thread. To open a new conversation, send:
- **agent_id:** the unique identifier you received when creating the agent
- **inputs:** the initial message (a string, either the user’s question or the conversation history)

The call will return a **conversation_id**.

To continue the dialogue and append to the history, simply reuse:
- **conversation_id:** the ID of the ongoing conversation (stored server-side)
- **inputs:** the next message or response (either a string or a list of messages)

Each new message returns an updated **conversation_id**.

If you prefer not to automatically persist the history in our cloud, include store=false in your call; the exchange will not be saved.

Finally, the handoff_execution parameter controls the transfer mode:
- **`server` (default):** the handoff is executed automatically on our cloud
- **`client`:** the user receives the handoff response directly and handles it themselves.

<br>

___

## Text generation

First, let’s take a structured look at how to craft a request to the `v1/conversations` endpoint, whether in streaming or non-streaming mode.

___

### Non streamed

```Delphi
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;

  //Asynchronous promise example
  Start(TutorialHub);
  var Promise := Client.Conversations.AsyncAwaitCreate(
    procedure (Params: TConversationsParams)
    begin
      Params.Inputs('Explain to me what Joual represents for Quebecers.');
      Params.Model('mistral-medium-2505');
      Params.Store(False);
      Params.CompletionArgs(TCompletionArgsParams.Create
        .Temperature(0.3)
        .TopP(0.95)
      );
      TutorialHub.JSONRequest := Params.ToFormat();
    end);

  Promise
    .&Then<TConversation>(
      function (Value: TConversation): TConversation
      begin
        Result := Value;
        ConvId := Value.ConversationId;
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Asynchronous example
//  Client.Conversations.ASyncCreate(
//    procedure (Params: TConversationsParams)
//    begin
//      Params.Inputs('Explain to me what Joual represents for Quebecers.');
//      Params.Model('mistral-medium-2505');
//      Params.Store(False);
//      Params.CompletionArgs(TCompletionArgsParams.Create
//        .Temperature(0.3)
//        .TopP(0.95)
//      );
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end,
//    function : TAsyncConversation
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Value := Client.Conversations.Create(
//    procedure (Params: TConversationsParams)
//    begin
//      Params.Inputs('Explain to me what Joual represents for Quebecers.');
//      Params.Model('mistral-medium-2505');
//      Params.Store(False);
//      Params.CompletionArgs(TCompletionArgsParams.Create
//        .Temperature(0.3)
//        .TopP(0.95)
//      );
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```  

<br>

By using the MistralAI.Tutorial.VCL unit along with the initialization described [above](https://github.com/MaxiDonkey/DelphiMistralAI#strategies-for-quickly-using-the-code-examples), you can achieve results similar to the example shown below.

![Preview](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/sample/ConversationxNS.png?raw=true "Preview")

<br>

___

### Streamed

```Delphi
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;

  //Asynchronous promise example
  var Promise := Client.Conversations.AsyncAwaitCreateStream(
    procedure (Params: TConversationsParams)
    begin
      Params.Inputs('Explain to me what Joual represents for Quebecers.');
      Params.Model('mistral-medium-2505');
      Params.Store(False);
      Params.Stream;
      Params.CompletionArgs(TCompletionArgsParams.Create
        .Temperature(0.3)
        .TopP(0.95)
      );
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

  //Asynchronous example
//  Client.Conversations.AsyncCreateStream(
//    procedure (Params: TConversationsParams)
//    begin
//      Params.Inputs('Explain to me what Joual represents for Quebecers.');
//      Params.Model('mistral-medium-2505');
//      Params.Store(False);
//      Params.Stream;
//      Params.CompletionArgs(TCompletionArgsParams.Create
//        .Temperature(0.3)
//        .TopP(0.95)
//      );
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end,
//    function : TAsyncConversationsEvent
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnProgress := DisplayStream;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  Client.Conversations.CreateStream(
//    procedure(Params: TConversationsParams)
//    begin
//      Params.Inputs('Explain to me what Joual represents for Quebecers.');
//      Params.Model('mistral-medium-2505');
//      Params.Store(False);
//      Params.Stream;
//      Params.CompletionArgs(TCompletionArgsParams.Create
//        .Temperature(0.3)
//        .TopP(0.95)
//      );
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end,
//    procedure(var Chunk: TConversationsEvent; IsDone: Boolean; var Cancel: Boolean)
//    begin
//      if (not IsDone) and Assigned(Chunk) then
//        begin
//          DisplayStream(TutorialHub, Chunk);
//        end;
//    end);
```

<br>

By using the MistralAI.Tutorial.VCL unit along with the initialization described [above](https://github.com/MaxiDonkey/DelphiMistralAI#strategies-for-quickly-using-the-code-examples), you can achieve results similar to the example shown below.

![Preview](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/sample/ConversationxS.png?raw=true "Preview")

<br>

___

### Multi-turn conversations

We’ll outline the most efficient method for orchestrating a multi-turn exchange via the v1/conversation endpoint.

First, we’ll leverage conversation agents. For more details, please refer to the [Agents & Connectors](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/AgentsConnectors.md) guide.

The example below illustrates only the ***asynchronous, Promise-based approach***; the other two implementation strategies are left to the reader’s discretion.

#### Operational Plan
- [Step 1](#step-1): Initialize the conversational agent
- [Step 2](#step-2): Send the initial request to the endpoint to start the exchange and retrieve the conversation_id
- [Step 3](#step-3): For subsequent turns, reuse the conversation_id to continue the dialogue

<br>

___

##### Step 1 
**Initialize the conversational agent**

```Delphi
var AgentId: string;

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
        AgentId := Value.Id;  //e.g. ag_01985efe4b657539a2d6380dad6c3c55
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
```
>[!NOTE]
> You can get the list of active agents by referring to the [Agents & Connectors](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/AgentsConnectors.md) 

<br>

___
 
##### Step 2
**Send the initial request to the endpoint to start the exchange and retrieve the conversation_id**

```Delphi
  var Promise := Client.Conversations.AsyncAwaitCreateStream(
     procedure (Params: TConversationsParams)
    begin
      Params
        .Inputs('my_text_1')
        .AgentId(AgentId); //e.g. ag_01985efe4b657539a2d6380dad6c3c55
        .Stream
        .Store;  //Use the storing
      TutorialHub.JSONRequest := Params.ToFormat();
    end,
    function : TPromiseConversationsEvent
    begin
      Result.Sender := TutorialHub;
      Result.OnProgress :=
        procedure (Sender: TObject; Event: TConversationsEvent)
        begin
          if Event.&Type = TChunkEvent.conversation_response_started then
            ConvId := Event.ConversationId; //IMPORTANT : Get then conversation Id
          DisplayStream(Sender, Event);
        end;
      Result.OnDoCancel := DoCancellation;
      Result.OnCancellation :=
        function (Sender: TObject): string
        begin
          Cancellation(Sender);
        end;
    end);
```

>[!NOTE]
>We have now retrieved the conversation ID, which will allow us to continue the conversation by supplying it in our subsequent interactions.

<br>

___
 
##### Step 3
**For subsequent turns, reuse the conversation_id to continue the dialogue**

We can continue the conversation by using the Append or AppendStream methods, as well as their asynchronous variants.

```Delphi
  var Promise := Client.Conversations.AsyncAwaitAppendStream(ConvId,
    procedure(Params: TConversationsParams)
    begin
      Params.Inputs('my_text_2');
      Params.Stream;
      Params.Store;  //Use the storing
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
```

<br>

___

### Parallel method for generating text

This approach enables the simultaneous execution of multiple prompts, provided they are all processed by the same model. It also supports parallel web requests.

#### Example : Two prompts processed in parallel.

```Delphi
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  Client.HttpClient.ResponseTimeout := 120000;

  //Asynchronous promise example
  Start(TutorialHub);
  var Promise := Client.Conversations.AsyncAwaitCreateParallel(
    procedure (Params: TBundleParams)
    begin
      Params.Prompts([
        'How many television channels were there in France in 1980?',
        'How many TV channels were there in Germany in 1980?.'
      ]);
      Params.System('Write the response in capital letters.');
      Params.Model('mistral-medium-2505');
    end);

  Promise
    .&Then<TBundleList>(
      function (Value: TBundleList): TBundleList
      begin
        Result := Value;
        for var Item in Value.Items do
          begin
            Display(TutorialHub, F('index', Item.Index.ToString));
            Display(TutorialHub, TConversation(Item.Chat).Outputs[0].Content[0].Text);
            Display(TutorialHub, EmptyStr);
          end;
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Asynchronous example
//  Client.Conversations.CreateParallel(
//    procedure (Params: TBundleParams)
//    begin
//      Params.Prompts([
//        'How many television channels were there in France in 1980?',
//        'How many TV channels were there in Germany in 1980?.'
//      ]);
//      Params.System('Write the response in capital letters.');
//      Params.Model('mistral-medium-2505');
//    end,
//    function : TAsynBundleList
//    begin
//      Result.Sender := TutorialHub;
//
//      Result.OnStart :=
//        procedure (Sender: TObject)
//        begin
//          Display(Sender, 'Start the job' + sLineBreak);
//        end;
//
//      Result.OnSuccess :=
//        procedure (Sender: TObject; Bundle: TBundleList)
//        begin
//          // Background bundle processing
//          for var Item in Bundle.Items do
//            begin
//              Display(Sender, 'Index : ' + Item.Index.ToString);
//              Display(Sender, 'FinishIndex : ' + Item.FinishIndex.ToString);
//              Display(Sender, Item.Prompt + sLineBreak);
//              Display(Sender, Item.Response + sLineBreak + sLineBreak);
//              // or Display(Sender, TChat(Item.Chat).Choices[0].Message.Content);
//            end;
//        end;
//
//      Result.OnError := Display;
//    end);
```

<br>

___

### Reasoning

The implementation of reasoning with the `v1/conversations` endpoint differs from that of `v1/chat/completions`. In particular, the `v1/conversations` API does not incorporate tokenized “thought” blocks via control tokens; consequently, there is no explicit trace of reflection within the content segments.

```Delphi
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;

  //Asynchronous promise example
  var Promise := Client.Conversations.AsyncAwaitCreateStream(
    procedure (Params: TConversationsParams)
    begin
      Params.Model('magistral-small-latest');
      Params.Instructions;
      Params.Inputs('John is one of 4 children. The first sister is 4 years old. Next year, the second sister will be twice as old as the first sister. The third sister is two years older than the second sister. The third sister is half the age of her older brother. How old is John?');
      Params.Store(False);
      Params.Stream;
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

  //Asynchronous example
//  Client.Conversations.AsyncCreateStream(
//    procedure (Params: TConversationsParams)
//    begin
//      Params.Model('magistral-small-latest');
//      Params.Instructions;
//      Params.Inputs('John is one of 4 children. The first sister is 4 years old. Next year, the second sister will be twice as old as the first sister. The third sister is two years older than the second sister. The third sister is half the age of her older brother. How old is John?');
//      Params.Store(False);
//      Params.Stream;
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end,
//    function : TAsyncConversationsEvent
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnProgress := DisplayStream;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  Client.Conversations.CreateStream(
//    procedure (Params: TConversationsParams)
//    begin
//      Params.Model('magistral-small-latest');
//      Params.Instructions;
//      Params.Inputs('John is one of 4 children. The first sister is 4 years old. Next year, the second sister will be twice as old as the first sister. The third sister is two years older than the second sister. The third sister is half the age of her older brother. How old is John?');
//      Params.Store(False);
//      Params.Stream;
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end,
//    procedure(var Chunk: TConversationsEvent; IsDone: Boolean; var Cancel: Boolean)
//    begin
//      if (not IsDone) and Assigned(Chunk) then
//        begin
//          DisplayStream(TutorialHub, Chunk);
//        end;
//    end);
```

>[!NOTE]
> The string `ReasoningEnglishInstructions` is available in the `MistralAI.Types` unit.

<br>

___

## Conversations managment

### Continue a conversation

#### Non streamed

Refer to the [official documentation](https://docs.mistral.ai/api/#tag/beta.conversations/operation/agents_api_v1_conversations_append)

```Delphi
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  ConvId := 'conversation_id'; //e.g. conv_0198261f353271a4bd9e95ef32105994

  //Asynchronous promise example
  var Promise := Client.Conversations.AsyncAwaitAppend(ConvId,
    procedure (Params: TConversationsParams)
    begin
      Params.Inputs('In what proportion of the Canadian population is joual used?');
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

  //Asynchronous example
//  Client.Conversations.AsyncAppend(ConvId,
//    procedure (Params: TConversationsParams)
//    begin
//      Params.Inputs('In what proportion of the Canadian population is joual used?);
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end,
//    function : TAsyncConversation
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Value := Client.Conversations.Append(ConvId,
//    procedure (Params: TConversationsParams)
//    begin
//      Params.Inputs('In what proportion of the Canadian population is joual used?);
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

#### Streamed

Refer to the [official documentation](https://docs.mistral.ai/api/#tag/beta.conversations/operation/agents_api_v1_conversations_append_stream)

```Delphi
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  ConvId := 'conversation_id'; //e.g. conv_0198261f353271a4bd9e95ef32105994 

  //Asynchronous promise example
  var Promise := Client.Conversations.AsyncAwaitAppendStream(ConvId,
    procedure(Params: TConversationsParams)
    begin
      Params.Inputs('In what proportion of the Canadian population is joual used?');
      Params.Stream;
      Params.Store;
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
         ShowMessage(Value);
       end)
    .Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Asynchronous example
//  Client.Conversations.AsyncAppendStream(ConvId,
//    procedure(Params: TConversationsParams)
//    begin
//      Params.Inputs('In what proportion of the Canadian population is joual used?');
//      Params.Stream;
//      Params.Store;
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end,
//    function : TAsyncConversationsEvent
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnProgress := DisplayStream;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  Client.Conversations.AppendStream(ConvId,
//    procedure(Params: TConversationsParams)
//    begin
//      Params.Inputs('In what proportion of the Canadian population is joual used?');
//      Params.Stream;
//      Params.Store;
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end,
//    procedure(var Chunk: TConversationsEvent; IsDone: Boolean; var Cancel: Boolean)
//    begin
//      if (not IsDone) and Assigned(Chunk) then
//        begin
//          DisplayStream(TutorialHub, Chunk);
//        end;
//    end);
```

<br>

___

### Retrieve conversations

Refer to the [official documentation](https://docs.mistral.ai/api/#tag/beta.conversations/operation/agents_api_v1_conversations_get)

```Delphi
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  ConvId := 'conversation_id'; //e.g. conv_0198261f353271a4bd9e95ef32105994
  
  //Asynchronous promise example
  var Promise := Client.Conversations.AsyncAwaitRetrieve(ConvId);

  Promise
    .&Then<TConversationsListItem>(
       function (Value: TConversationsListItem): TConversationsListItem
       begin
         Result := Value;
         Display(TutorialHub, Value);
       end)
    .Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Asynchronous example
//  Client.Conversations.AsyncRetrieve(ConvId,
//    function : TAsyncConversationsListItem
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Value := Client.Conversations.Retrieve(ConvId);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br>

___

### Restart a conversation

#### Non streamed

Refer to the [official documentation](https://docs.mistral.ai/api/#tag/beta.conversations/operation/agents_api_v1_conversations_restart)

```Delphi
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  ConvId := 'conversation_id'; //e.g. conv_0198261f353271a4bd9e95ef32105994
  
  //Asynchronous promise example
  var Promise := Client.Conversations.AsyncAwaitRestart(ConvId,
    procedure (Params: TConversationsParams)
    begin
      Params.Inputs('Name some dialects of French, such as Joual.');
      Params.FromEntryId(Restart_id); //e.g. msg_019835d44cd47796b0066c6e10951eac
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

  //Asynchronous example
//  Client.Conversations.AsyncRestart(ConvId,
//    procedure (Params: TConversationsParams)
//    begin
//      Params.Inputs('Name some dialects of French, such as Joual.');
//      Params.FromEntryId(Restart_id); //e.g. msg_019835d44cd47796b0066c6e10951eac
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end,
//    function : TAsyncConversation
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Value := Client.Conversations.Restart(ConvId,
//    procedure (Params: TConversationsParams)
//    begin
//      Params.Inputs('Name some dialects of French, such as Joual.');
//      Params.FromEntryId(Restart_id); //e.g. msg_019835d44cd47796b0066c6e10951eac
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

#### Streamed

Refer to the [official documentation](https://docs.mistral.ai/api/#tag/beta.conversations/operation/agents_api_v1_conversations_restart_stream)

```Delphi
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  ConvId := 'conversation_id'; //e.g. conv_0198261f353271a4bd9e95ef32105994
  
  //Asynchronous promise example
  var Promise := Client.Conversations.AsyncAwaitRestartStream(ConvId,
    procedure (Params: TConversationsParams)
    begin
      Params.Inputs('Name some dialects of French, such as Joual.l');
      Params.FromEntryId(Restart_id); //e.g. msg_019835d44cd47796b0066c6e10951eac
      Params.Stream;
      Params.Store;
      Params.HandoffExecution('server');
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
         ShowMessage(Value);
       end)
    .Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Asynchronous example
//  Client.Conversations.AsyncRestartStream(ConvId,
//    procedure (Params: TConversationsParams)
//    begin
//      Params.Inputs('Name some dialects of French, such as Joual.');
//      Params.FromEntryId(Restart_id); //e.g. msg_019835d44cd47796b0066c6e10951eac
//      Params.Stream;
//      Params.Store;
//      Params.HandoffExecution('server');
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end,
//    function : TAsyncConversationsEvent
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnProgress := DisplayStream;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  Client.Conversations.RestartStream(ConvId,
//    procedure (Params: TConversationsParams)
//    begin
//      Params.Inputs('Name some dialects of French, such as Joual.');
//      Params.FromEntryId(Restart_id); //e.g. msg_019835d44cd47796b0066c6e10951eac
//      Params.Stream;
//      Params.Store;
//      Params.HandoffExecution('server');
//      TutorialHub.JSONRequest := Params.ToFormat();
//    end,
//    procedure(var Chunk: TConversationsEvent; IsDone: Boolean; var Cancel: Boolean)
//    begin
//      if (not IsDone) and Assigned(Chunk) then
//        begin
//          DisplayStream(TutorialHub, Chunk);
//        end;
//    end);
```

<br>

___


### List all created conversations

Refer to the [official documentation](https://docs.mistral.ai/api/#tag/beta.conversations/operation/agents_api_v1_conversations_list)

```Delphi
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;

  //Asynchronous promise example
  var Promise := Client.Conversations.AsyncAwaitList(
    procedure (Params: TConversationsListParams)
    begin
      Params.Page(0);
      Params.PageSize(100);
    end);

  Promise
    .&Then<TConversationsList>(
      function (Value: TConversationsList): TConversationsList
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
//  Client.Conversations.AsyncList(
//    procedure (Params: TConversationsListParams)
//    begin
//      Params.Page(0);
//      Params.PageSize(100);
//    end,
//    function : TAsyncConversationsList
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Value := Client.Conversations.List(
//    procedure (Params: TConversationsListParams)
//    begin
//      Params.Page(0);
//      Params.PageSize(100);
//    end);
//
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br>

___

### Retrieve all entries in a conversation

Refer to the [official documentation](https://docs.mistral.ai/api/#tag/beta.conversations/operation/agents_api_v1_conversations_history)

```Delphi
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  ConvId := 'conversation_id'; //e.g. conv_0198261f353271a4bd9e95ef32105994
  
  //Asynchronous promise example
  var Promise := Client.Conversations.AsyncAwaitGetHistory(ConvId);

  Promise
    .&Then<TRetrievedEntries>(
       function (Value: TRetrievedEntries): TRetrievedEntries
       begin
         Result := Value;
         Display(TutorialHub, Value);
       end)
    .Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Asynchronous example
//  Client.Conversations.AsyncGetHistory(ConvId,
//    function : TAsyncRetrievedEntries
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Value := Client.Conversations.GetHistory(ConvId);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br>

___

### Retrieve all messages in a conversation

Refer to the [official documentation](https://docs.mistral.ai/api/#tag/beta.conversations/operation/agents_api_v1_conversations_messages)

```Delphi
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  ConvId := 'conversation_id'; //e.g. conv_0198261f353271a4bd9e95ef32105994
  
  //Asynchronous promise example
  var Promise := Client.Conversations.AsyncAwaitGetMessages(ConvId);

  Promise
    .&Then<TRetrieveMessages>(
       function (Value: TRetrieveMessages): TRetrieveMessages
       begin
         Result := Value;
         Display(TutorialHub, Value);
       end)
    .Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Asynchronous example
//  Client.Conversations.AsyncGetMessages(ConvId,
//    function : TAsyncRetrieveMessages
//    begin
//      Result.Sender := TutorialHub;
//      Result.OnStart := Start;
//      Result.OnSuccess := Display;
//      Result.OnError := Display;
//    end);

  //Synchronous example
//  var Value := Client.Conversations.GetMessages(ConvId);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

<br>

___

## Tools for conversations

Connectors are tools that conversations—or the agents driving them—can invoke at any point to fulfill requests. They are deployed and available for on-demand use within agent workflows.

Additionally, connectors can be called directly by users through the Conversations API without the need to create an agent first.

There are four primary tools available:
- **Web Search**
- **Code Interpreter**
- **Image Generation**
- **Document Library**

Additionally, **reasoning** can be treated as a de facto tool in its own right.

We do not provide code snippets that demonstrate direct use of the tools within conversations; instead, those tool-specific snippets are exposed via agents. Since a conversation can delegate to agents, this simplifies the tutorial.

Please refer to the [Conversation & Agents](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/AgentsConnectors.md) section.

<br>

___

## Function calling

The step-by-step process is already covered in the [Function calling](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/ChatCompletion.md#function-calling) section of the chat/completions chapter. Refer to that section for the general approach to invoking a function with DelphiMistralAI.
Here, we will only outline the specific differences applicable when using the v1/conversations endpoint.

 - [Display a stream text](#display-a-stream-text) 
 - [The main method](#the-main-method)
 - [The FinishReason](#the-finishreason)

<br>

___

### Display a stream text

```Delphi
procedure TVCLTutorialHub.WeatherFunctionEx(const Value: TMessageOutputEntry;
  Func: IFunctionCore);
begin
  var ArgResult := Func.Execute(Value.Arguments);

  FClient.Conversations.AsyncCreateStream(
    procedure (Params: TConversationsParams)
    begin
      Params.Instructions('Respond like a star weather presenter on a prime-time TV channel.');
      Params.Inputs(ArgResult);
      Params.Model('mistral-medium-2505');
      Params.Stream;
      Params.Store(False);
      Params.CompletionArgs(TCompletionArgsParams.Create
        .Temperature(0.3)
        .TopP(0.95)
      )
    end,
    function : TAsyncConversationsEvent
    begin
      Result.Sender := TutorialHub;
      Result.OnProgress := DisplayStream;
      Result.OnSuccess := Display;
      Result.OnDoCancel := DoCancellation;
      Result.OnCancellation := Cancellation;
      Result.OnError := Display;
    end);
end;  
```
>[!NOTE]
>The same method also exists with the FMX version of the TutorialHub wizard.

<br>

___

### The main method

Building the query using the Weather tool. (Simply copy/paste this last code to test the usage of the functions.)

```Delphi
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL, MistralAI.Functions.Example or Mistral.Tutorial.FMX;

  TutorialHub.JSONRequestClear;
  var WeatherFunc: IFunctionCore := TWeatherReportFunction.Create;

  TutorialHub.Tool := WeatherFunc;
  TutorialHub.ToolCallEx := TutorialHub.WeatherFunctionEx;

  //Asynchronous example
  Client.Conversations.ASyncCreate(
    procedure (Params: TConversationsParams)
    begin
      Params.Model('mistral-large-latest');
      Params.Inputs('What''s the weather like in Paris temperature in celcius?');
      Params.Tools([TConnectorParams.New(WeatherFunc)]);
      TutorialHub.JSONRequest := Params.ToFormat();
    end,
    function : TAsyncConversation
    begin
      Result.Sender := TutorialHub;
      Result.OnStart := Start;
      Result.OnSuccess := Display;
      Result.OnError := Display;
    end);
```

<br>

___

### The FinishReason

Let's look at how the display method handles the function call.

```Delphi
procedure Display(Sender: TObject; Value: TConversation);
begin
  TutorialHub.JSONResponse := Value.JSONResponse;
  for var Item in Value.Outputs do
  begin
    case Item.&Type of
      TConversatonEvent.function_call :
        TutorialHub.ToolCallEx(Item, TutorialHub.Tool);  //call the function execution

      TConversatonEvent.message_input,
      TConversatonEvent.message_output:
        begin
          for var SubItem in Item.Content do
            case SubItem.&Type of
              TContentChunkType.text:
                Display(Sender, SubItem.Text);

              TContentChunkType.tool_file:
                begin
                  case Subitem.Tool of
                    TConversationTool.image_generation:
                      TutorialHub.LoadImage(SubItem.FileId);
                  end;
                end;
            end;
        end;
    end;
  end;
end;
```

>[!CAUTION]
>Ensure user confirmation for actions like sending emails or making purchases to avoid unintended consequences.

<br>

___

## Structured Output

 - [Custom Structured Outputs](#custom-structured-outputs)
 - [JSON mode](#json-mode)

### Custom Structured Outputs

To activate JSON mode, set response_format to {"type":"json_object"}. This JSON output option is now supported for every model via our API.

```Delphi
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;

  var Schema :=
    '{' +
    '    "schema": {' +
    '      "properties": {' +
    '        "name": {' +
    '          "title": "Name",' +
    '          "type": "string"' +
    '        },' +
    '        "authors": {' +
    '          "items": {' +
    '            "type": "string"' +
    '          },' +
    '          "title": "Authors",' +
    '          "type": "array"' +
    '        }' +
    '      },' +
    '      "required": ["name", "authors"],' +
    '      "title": "Book",' +
    '      "type": "object",' +
    '      "additionalProperties": false' +
    '    },' +
    '    "name": "book",' +
    '    "strict": true' +
    '  }';

  //Asynchronous promise example
  Start(TutorialHub);
  var Promise := Client.Conversations.AsyncAwaitCreate(
    procedure (Params: TConversationsParams)
    begin
      Params.Model('mistral-large-latest');
      Params.Instructions('Extract the books information.');
      Params.Inputs('I recently read To Kill a Mockingbird by Harper Lee.');
      Params.CompletionArgs(
        TCompletionArgsParams.Create
          .ResponseFormat(Schema)
      );
      Params.Store(False);
      TutorialHub.JSONRequest := Params.ToFormat();
    end);

  promise
    .&Then<string>(
      function (Value: TConversation): string
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
```

**Example output:**

```Json
{
 "name": "To Kill a Mockingbird",
 "authors": [
  "Harper Lee"
 ]
}
```

<br>

___

### JSON mode

By defining a strict JSON schema up front, Custom Structured Outputs compel the model to emit responses that match your exact structure—right down to field names and data types. In practice, this means you get reliably formatted JSON every time, with the correct keywords and types baked in.

Refer to the [official documentation](https://docs.mistral.ai/capabilities/structured-output/custom_structured_output/)

```Delphi
//uses MistralAI, MistralAI.Types, MistralAI.Tutorial.VCL or MistralAI.Tutorial.FMX;

  TutorialHub.JSONRequestClear;

  //Asynchronous promise example
  Start(TutorialHub);
  var Promise := Client.Conversations.AsyncAwaitCreate(
    procedure (Params: TConversationsParams)
    begin
      Params.Model('mistral-large-latest');
      Params.Inputs('What is the best French meal? Return the name and the ingredients in short JSON object.');
      Params.CompletionArgs(
        TCompletionArgsParams.Create
          .ResponseFormat(TResponseFormatParams.Json_Oject)
      );
      Params.Store(False);
      TutorialHub.JSONRequest := Params.ToFormat();
    end);

  promise
    .&Then<string>(
      function (Value: TConversation): string
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
```

**Example output:**

```Json
{
  "name": "Boeuf Bourguignon",
  "ingredients": [
    "beef",
    "red wine",
    "carrots",
    "onions",
    "garlic",
    "bacon",
    "mushrooms",
    "herbs",
    "beef stock"
  ]
}
```

<br>

___

## Agents & connectors

An agent is a preconfigured entity that bundles a model, instructions, and tools to enrich and steer the model’s capabilities.
It’s used to automate and tailor interactions by supplying context and dedicated functionality without having to reconfigure for each request.

Follow the link to learn [how to create agents and use connectors](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/AgentsConnectors.md).