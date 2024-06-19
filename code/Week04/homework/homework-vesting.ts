import {
    Data,
    Lucid,
    Blockfrost,
    getAddressDetails,
    SpendingValidator,
    TxHash,
    Datum,
    UTxO,
    Address,
    AddressDetails,
} from "https://deno.land/x/lucid@0.9.1/mod.ts"
// create a seed.ts file with your seed
//TODO create 2 seed files
import { secretSeed } from "./seed.ts"
import { secretSeed2 } from "./seed2.ts"

// set blockfrost endpoint
const lucid = await Lucid.new(
  new Blockfrost(
    "https://cardano-preprod.blockfrost.io/api/v0",
    "preprodN5IDBsioNNKutjNxqxSeYpddGpRkCXJX"
  ),
  "Preprod"
);

// load local stored seed as a wallet into lucid
lucid.selectWalletFromSeed(secretSeed2);
const addr2: Address = await lucid.wallet.address();
console.log("Address 2: " + addr2);

// load local stored seed as a wallet into lucid
lucid.selectWalletFromSeed(secretSeed);
const addr: Address = await lucid.wallet.address();
console.log("Address 1: " + addr);

// Define the vesting plutus script
//TODO replace script
const vestingScript: SpendingValidator = {
    type: "PlutusV2",
    script: "590ba1590b9e010000323232332232323332223232323232323233223233223232323232333222323232323232323232322323232223232533532323232325335533553353300e500235005222003133350105017335014335016502d0323350153502e35005222001032500110311032133573892012442656e69666963696172793120646964206e6f74207369676e206f7220746f206c6174650003110321533553353300e5002350052220021333501050173350143350163502e3370090011a80291100081919a80aa816019280088188819099ab9c49012842656e69666963696172793220646964206e6f74207369676e206f7220697320746f206561726c79000311355001222222222222005135001220023333573466e1cd55cea80224000466442466002006004646464646464646464646464646666ae68cdc39aab9d500c480008cccccccccccc88888888888848cccccccccccc00403403002c02802402001c01801401000c008cd409c0a0d5d0a80619a8138141aba1500b33502702935742a014666aa056eb940a8d5d0a804999aa815bae502a35742a01066a04e0686ae85401cccd540ac0d5d69aba150063232323333573466e1cd55cea80124000466a0486464646666ae68cdc39aab9d5002480008cd40a8cd40fdd69aba150023043357426ae8940088c98c8114cd5ce02402382189aab9e5001137540026ae854008c8c8c8cccd5cd19b8735573aa0049000119a81499a81fbad35742a00460866ae84d5d1280111931902299ab9c048047043135573ca00226ea8004d5d09aba2500223263204133573808808607e26aae7940044dd50009aba1500533502775c6ae854010ccd540ac0c48004d5d0a801999aa815bae200135742a00460666ae84d5d1280111931901e99ab9c04003f03b135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d55cf280089baa00135742a00860466ae84d5d1280211931901799ab9c03203102d3333573466e1cd55ce9baa0054800080c08c98c80b8cd5ce0188180161999ab9a3370e6aae75401d2000233322212333001004003002375c6ae85401cdd71aba15006375a6ae84d5d1280311931901699ab9c03002f02b102e13263202c3357389201035054350002e135573ca00226ea80044d5d1280089aab9e500113754002446a004444444444444a66a666aa601e2400264246600244a66a004420062002004a0464a66a666ae68cdc780700081981909a8128008a812002108198818990009aa8131108911299a80089a80191000910999a802910011802001199aa98038900080280200089111a801111a801111a802911a801112999a999a8068058030010a99a8008a99a8028999a80600580180388160999a80600580180388160999a80600580180389111a801111a801912999a999a8040038020010a99a80188008814081388140911191919192999a80310a999a80310a999a80410980224c26006930a999a80390980224c2600693080888078a999a80390980224c26006930a999a80310980224c260069308080a999a80290807080788068a999a80290a999a803909802a4c26008930a999a803109802a4c2600893080808070a999a803109802a4c26008930a999a802909802a4c2600893080792999a80290a999a80390a999a80390999a8058050010008b0b0b08078a999a80310a999a80310999a8050048010008b0b0b0807080692999a80210a999a80310a999a80310999a8050048010008b0b0b08070a999a80290a999a80290999a8048040010008b0b0b0806880612999a80190a999a80290a999a80290999a8048040010008b0b0b08068a999a80210a999a80210999a8040038010008b0b0b0806080592999a80110a999a80210a999a80210999a8040038010008b0b0b08060a999a80190a999a80190999a8038030010008b0b0b08058805091a800911111110038910919800801801091091980080180109109198008018010891999999980091199ab9a3370e00400203c03a44a66a666ae68cdc380100080f00e88030a99a999ab9a3371200400203c03a2008200a44666ae68cdc400100080f00e91199ab9a3371200400203c03a44666ae68cdc480100080e80f11199ab9a3371000400203a03c44a66a666ae68cdc480100080f00e8800880111299a999ab9a3371200400203c03a200420022444006244400424440022464460046eb0004c8004d5406c88cccd55cf80092805119a80498021aba1002300335744004036464646666ae68cdc39aab9d5002480008cc8848cc00400c008c038d5d0a80118029aba135744a004464c6403266ae7007006c05c4d55cf280089baa0012323232323333573466e1cd55cea8022400046666444424666600200a0080060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008c05cd5d0a80119a80780b1aba135744a004464c6403c66ae700840800704d55cf280089baa00135742a008666aa010eb9401cd5d0a8019919191999ab9a3370ea0029002119091118010021aba135573ca00646666ae68cdc3a80124004464244460020086eb8d5d09aab9e500423333573466e1d400d20002122200323263202033573804604403c03a03826aae7540044dd50009aba1500233500b75c6ae84d5d1280111931900d19ab9c01d01c018135744a00226ae8940044d55cf280089baa0011335500175ceb44488c88c008dd5800990009aa80c11191999aab9f00225008233500733221233001003002300635573aa004600a6aae794008c010d5d100180c89aba100111220021221223300100400312232323333573466e1d400520002350073005357426aae79400c8cccd5cd19b875002480089401c8c98c8054cd5ce00c00b80980909aab9d5001137540022424460040062244002464646666ae68cdc3a800a400c46424444600800a600e6ae84d55cf280191999ab9a3370ea004900211909111180100298049aba135573ca00846666ae68cdc3a801a400446424444600200a600e6ae84d55cf280291999ab9a3370ea00890001190911118018029bae357426aae7940188c98c804ccd5ce00b00a80880800780709aab9d500113754002464646666ae68cdc39aab9d5002480008cc8848cc00400c008c014d5d0a8011bad357426ae8940088c98c803ccd5ce00900880689aab9e5001137540024646666ae68cdc39aab9d5001480008dd71aba135573ca004464c6401a66ae7004003c02c4dd5000919191919191999ab9a3370ea002900610911111100191999ab9a3370ea004900510911111100211999ab9a3370ea00690041199109111111198008048041bae35742a00a6eb4d5d09aba2500523333573466e1d40112006233221222222233002009008375c6ae85401cdd71aba135744a00e46666ae68cdc3a802a400846644244444446600c01201060186ae854024dd71aba135744a01246666ae68cdc3a8032400446424444444600e010601a6ae84d55cf280591999ab9a3370ea00e900011909111111180280418071aba135573ca018464c6402c66ae7006406005004c04804404003c0384d55cea80209aab9e5003135573ca00426aae7940044dd50009191919191999ab9a3370ea002900111999110911998008028020019bad35742a0086eb4d5d0a8019bad357426ae89400c8cccd5cd19b875002480008c8488c00800cc020d5d09aab9e500623263200f33573802402201a01826aae75400c4d5d1280089aab9e500113754002464646666ae68cdc3a800a400446424460020066eb8d5d09aab9e500323333573466e1d400920002321223002003375c6ae84d55cf280211931900619ab9c00f00e00a009135573aa00226ea8004488c8c8cccd5cd19b87500148010940188cccd5cd19b875002480088d4024c018d5d09aab9e500423333573466e1d400d20002500923263200d33573802001e01601401226aae7540044dd5000890911180180208911001089110009191999ab9a3370ea0029001100311999ab9a3370ea0049000100311931900319ab9c009008004003135573a6ea8005261220021220011200149010350543100112323001001223300330020020011",
};
const vestingAddress: Address = lucid.utils.validatorToAddress(vestingScript);

// Create the vesting datum type
const VestingDatum = Data.Object({
    preDeadlineBeneficary: Data.String,
    postDeadlineBeneficary: Data.String, 
    deadline: Data.BigInt,
});
type VestingDatum = Data.Static<typeof VestingDatum>;

// Set the vesting deadline
const deadlineDate: Date = new Date("2023-03-19T00:00:00Z")
const deadlinePosIx = BigInt(deadlineDate.getTime());

// Set the vesting beneficiary to our own key.
const postDeadlineBeneficiaryDetails: AddressDetails = getAddressDetails(addr2);
const postDeadlineBeneficiaryPKH: string = postDeadlineBeneficiaryDetails.paymentCredential.hash

// set the second vesting beneficiary
const preDeadlineBeneficary: AddressDetails = getAddressDetails(addr);
const preDeadlineBeneficiaryPKH: string = preDeadlineBeneficary.paymentCredential.hash

// Creating a datum with a beneficiary and deadline
const datum: VestingDatum = {
    preDeadlineBeneficary: preDeadlineBeneficiaryPKH,
    postDeadlineBeneficary: postDeadlineBeneficiaryPKH,
    deadline: deadlinePosIx,
};

// An asynchronous function that sends an amount of Lovelace to the script with the above datum.
async function vestFunds(amount: bigint): Promise<TxHash> {
    const dtm: Datum = Data.to<VestingDatum>(datum,VestingDatum);
    const tx = await lucid
      .newTx()
      .payToContract(vestingAddress, { inline: dtm }, { lovelace: amount })
      .complete();
    const signedTx = await tx.sign().complete();
    const txHash = await signedTx.submit();
    return txHash
}

//TODO add second claim fucntion for pre deadline beneficiary
async function claimVestedFundsAfterDeadline(): Promise<TxHash> {
    const dtm: Datum = Data.to<VestingDatum>(datum,VestingDatum);
    const utxoAtScript: UTxO[] = await lucid.utxosAt(vestingAddress);
    const ourUTxO: UTxO[] = utxoAtScript.filter((utxo) => utxo.datum == dtm);
    
    if (ourUTxO && ourUTxO.length > 0) {
        const tx = await lucid
            .newTx()
            .collectFrom(ourUTxO, Data.void())
            .addSignerKey(postDeadlineBeneficiaryPKH)
            .attachSpendingValidator(vestingScript)
            .validFrom(Date.now()-100000)
            .complete();

        const signedTx = await tx.sign().complete();
        const txHash = await signedTx.submit();
        return txHash
    }
    else return "No UTxO's found that can be claimed"
}

console.log(await vestFunds(10000n));
//console.log(await claimVestedFundsAfterDeadline());