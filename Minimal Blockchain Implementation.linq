<Query Kind="FSharpProgram">
  <Namespace>System.Security.Cryptography</Namespace>
  <RuntimeVersion>6.0</RuntimeVersion>
</Query>

// https://towardsdatascience.com/building-a-minimal-blockchain-in-python-4f2e9934101d

type Block = 
    { Index: int
      Timestamp: DateTimeOffset 
      Data: string 
      PreviousHash: string option
      Hash: string }

type BlockChain = 
    { Blocks: Block array }

type BlockVerificationResult = 
    { Valid: bool
      Description: string option
      Block: Block }
      
type BlockChainVerificationResult = 
    { Valid: bool
      InvalidBlocks: BlockVerificationResult array }
      
let hashOrEmpty hash =
    hash |> Option.defaultValue String.Empty
      
let createHash index (timestamp: DateTimeOffset) data previousHash =
    let h = previousHash |> hashOrEmpty
    let s = sprintf "%i%i%s%s" index timestamp.UtcTicks data h
    let hashBytes = SHA256.HashData(Encoding.UTF8.GetBytes(s))
    BitConverter.ToString(hashBytes).Replace("-", "").ToLowerInvariant()

let getHash block =
    createHash block.Index block.Timestamp block.Data block.PreviousHash

let createBlockWithTimestamp getTimestamp index data previousHash = 
    let ts = getTimestamp()
    let hash = createHash index ts data previousHash
    { Index = index
      Timestamp = ts
      Data = data
      PreviousHash = previousHash
      Hash = hash }

let createBlock = 
    createBlockWithTimestamp (fun () -> DateTimeOffset.UtcNow)

let createChain = 
    let b = createBlock 0 "GENESIS" None
    { Blocks = [|b|] }
    
let addBlock data chain =
    let len = chain.Blocks.Length
    let previousHash = chain.Blocks[len - 1].Hash
    let block = createBlock len data (Some previousHash)
    let blocks = [|block|] |> Array.append chain.Blocks
    { Blocks = blocks }

let (|IndexInvalid|IndexValid|) index block = 
    if block.Index <> index then IndexInvalid else IndexValid
    
let (|TimestampInvalid|TimestampValid|) chain index block = 
    if index + 1 < chain.Blocks.Length && block.Timestamp.UtcTicks >= chain.Blocks[index + 1].Timestamp.UtcTicks then TimestampInvalid else TimestampValid

let (|HashInvalid|HashValid|) block = 
    if block.Hash <> (block |> getHash) then HashInvalid else HashValid

let (|PreviousHashInvalid|PreviousHashValid|) chain index block = 
    if index > 0 && (block.PreviousHash |> hashOrEmpty) <> chain.Blocks[index - 1].Hash then PreviousHashInvalid else PreviousHashValid

let verifyBlock (chain: BlockChain) (index: int) (block: Block) =
    let (|IndexInvalid|IndexValid|) = (|IndexInvalid|IndexValid|) index
    let (|TimestampInvalid|TimestampValid|) = (|TimestampInvalid|TimestampValid|) chain index
    let (|PreviousHashInvalid|PreviousHashValid|) = (|PreviousHashInvalid|PreviousHashValid|) chain index
    
    match block with
    | IndexInvalid ->
        { Valid = false
          Description = Some (sprintf "Incorrect block index (expected %i, was %i)" index block.Index)
          Block = block }
    | TimestampInvalid ->
        { Valid = false
          Description = Some (sprintf "Invalid timestamp at index %i (more recent than next block)" index)
          Block = block }
    | HashInvalid ->
        { Valid = false
          Description = Some (sprintf "Invalid hash at index %i (expected %s, was %s)" index (block |> getHash) block.Hash)
          Block = block }
    | PreviousHashInvalid ->
        { Valid = false
          Description = Some (sprintf "Invalid previous hash at index %i (expected %s, was %s)" index (block |> getHash) block.Hash)
          Block = block }
    | _ ->
        { Valid = true
          Description = None
          Block = block }
    
let verifyChain chain =
    let verify = verifyBlock chain

    let invalidBlocks = 
        chain.Blocks
        |> Array.mapi verify 
        |> Array.filter (fun r -> not r.Valid)
    
    if invalidBlocks.Length > 0 then
        { Valid = false; InvalidBlocks = invalidBlocks }
    else
        { Valid = true; InvalidBlocks = Array.empty }

// These are just to make visualisation easier, not required for function
type BlockForDisplay = 
    { Index: int
      Timestamp: string 
      Data: string 
      PreviousHash: string
      Hash: string }

type Block with 
    member this.ToDump() =
        { BlockForDisplay.Index = this.Index
          Timestamp = this.Timestamp.ToString("yyyy-MM-dd HH:mm:ss fffffff")
          Data = this.Data
          PreviousHash = this.PreviousHash |> hashOrEmpty
          Hash = this.Hash }

let bc1 = createChain
    
// bc1 |> Dump |> ignore

let bc2 = bc1 |> addBlock "TEST 1"

// bc2 |> Dump |> ignore

let bc3 = bc2 |> addBlock "TEST 2"

// bc3 |> Dump |> ignore

bc3 |> verifyChain |> Dump |> ignore

let badBlock = createBlock 1 "TEST 2" (Some "INVALIDHASH")

// Invalid timestamp
// bc3.Blocks[1] <- badBlock

// Invalid index
// bc3.Blocks[1] <- { badBlock with Index = 3 }

// Invalid hash
// bc3.Blocks[1] <- { badBlock with Timestamp = bc3.Blocks[1].Timestamp; Hash = "WRONGHASH" }

// Invalid previous hash
// bc3.Blocks[1] <- { badBlock with Timestamp = bc3.Blocks[1].Timestamp; Hash = (createHash badBlock.Index bc3.Blocks[1].Timestamp badBlock.Data badBlock.PreviousHash) }

bc3 |> Dump |> ignore

bc3 |> verifyChain |> Dump |> ignore

