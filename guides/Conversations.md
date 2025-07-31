# Conversations API (v1/conversations)

- [Introduction](#introduction)
- [Text generation](#text-generation)
    - [Non streamed](#non-streamed)
    - [Streamed](#streamed)
    - [Multi-turn conversations](#multi-turn-conversations) 
    - [Parallel method for generating text](#parallel-method-for-generating-text)
- [Vision](#vision)
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

First, let’s take a structured look at how to craft a request to the v1/conversations endpoint, whether in streaming or non-streaming mode.

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






## Vision


<br>

___

## Function calling


<br>

___

## Structured Output


<br>

___

## Agents & connectors

An agent is a preconfigured entity that bundles a model, instructions, and tools to enrich and steer the model’s capabilities.
It’s used to automate and tailor interactions by supplying context and dedicated functionality without having to reconfigure for each request.

Follow the link to learn [how to create agents and use connectors](https://github.com/MaxiDonkey/DelphiMistralAI/blob/main/guides/AgentsConnectors.md).