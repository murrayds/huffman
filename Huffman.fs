 //
 // Huffman.fs
 //
 // Author: Dakota Murray
 // Date  : 20 November, 2014
 // Class : CS 3490 - Programming Languages
 //
 // This class implements all methods necessary for the encoding and decoding of
 // strings in a huffman tree. A huffman tree allows for the encoding of textual
 // data using fewer bytes then straight ASCII encoding. This is accomplished by
 // representing more frequently used characters with fewer bits.
 //
 // The alphabet of a huffman tree is represented by a binary search tree, and
 // characters are represented by strings of binary numbers. Digits are pulled
 // off one by one and used tp traverse the tree. a '0' means "left" and a '1'
 // means "right". The appropriate branch is taken through the tree using the
 // '1' and '0' for direction. Upon hitting a leaf node, the character stored
 // in the leaf is returned.
 //
 // For the encoding process, all characters must be capital letters.
 //

#nowarn

module Huffman =

    //
    // zip
    //
    // Usage:       rec zip L1 L2
    // Description: Recursive function used to merge two lists into a single
    //              list of tuples where element 'i' of the tuple list is a
    //              tuple containing L1 and L2
    //
    // Params       L1 - A list to zip with L2
    //              L2 - A list to zip with L1
    //
    // Returns      A list of tuples, and each tuple i contains L1i and L2i
    //
    let rec zip L1 L2 =
        match L1 with
        | [] -> []
        | h1::t1 -> match L2 with
                    | [] -> []
                    | h2::t2 -> (h1,h2) :: (zip t1 t2)

    // A table of all ASCII characters
    let asciiTable =
        zip [for c in 'A'..'Z' do yield c] [for i in 65..90 do yield i]


    // A table containing huffman encodings of each ASCII character
    let huffmanTable =
      [ ('A',"1011");   ('B',"100000");   ('C',"01000");('D',"10101");    ('E',"110");
      ('F',"00000");  ('G',"100011");   ('H',"0110"); ('I',"1111");     ('J',"000011011");
      ('K',"0000111");('L',"10100");    ('M',"00011");('N',"1110");     ('O',"1001");
      ('P',"100001"); ('Q',"000011001");('R',"0101"); ('S',"0111");     ('T',"001");
      ('U',"01001");  ('V',"000010");   ('W',"00010");('X',"000011010");('Y',"100010");
      ('Z',"000011000")]

    // A type for defining nodes of the Huffman tree.
    //
    // A leaf node contains only a single character. Represents a leaf of the
    // tree
    //
    // A Node contains a tuple of two more huffmanTreeTypes. The first element
    // of the tuple is the 'Left' node, and the second is the 'right' node
    //
    type huffmanTreeType =
        | Leaf of char
        | Node of huffmanTreeType * huffmanTreeType


    // Sets up the huffmanTree for all characters
    let huffmanTree =
        Node(Node(Node(Node(Node(Leaf('F'),
                             Node(Leaf('V'),
                                  Node(Node(Node(Leaf('Z'),
                                                 Leaf('Q')),
                                            Node(Leaf('X'),
                                                 Leaf('J'))),
                                       Leaf('K')))),
                        Node(Leaf('W'),
                             Leaf('M'))),
                   Leaf('T')),
              Node(Node(Node(Leaf('C'),
                             Leaf('U')),
                        Leaf('R')),
                   Node(Leaf('H'),
                        Leaf('S')))),
         Node(Node(Node(Node(Node(Leaf('B'),
                                  Leaf('P')),
                             Node(Leaf('Y'),
                                  Leaf('G'))),
                        Leaf('O')),
                   Node(Node(Leaf('L'),
                             Leaf('D')),
                        Leaf('A'))),
              Node(Leaf('E'),
                   Node(Leaf('N'),
                        Leaf('I')))))

    //
    // explode
    //
    // Usage:       explode (str:string)
    // Description: Explodes a string into a list of characters.
    //
    // Params       str - The string to explode
    //
    // Returns      A list of characters exploded from the input string
    //
    let explode (str:string) =
        [for ch in str -> ch]

    //
    // implode
    //
    // Usage        rec implode lst
    // Desctiption: Implodes a list of characters into a single string.
    //
    // Params       lst - A list of characters to implode
    //
    // Returns      A string imploded from the list of characters
    //
    let rec implode lst =
        match lst with
        | [] -> ""
        | head :: tail -> new string(head, 1) + (implode tail)

    //
    // encodeChar
    //
    // Usage:       encodeChar table ch
    // Description: Encodes a single character into its huffman table binary
    //              value.
    //
    // Params       table - The huffmanTable list containing the ASCII binary
    //                      values
    //              ch - The character to encode
    //
    // Returns      The huffman tree encoded character
    //
    let rec encodeChar table ch =
        match table with
        | [] -> ""
        | head :: tail -> if (fst head) = ch then (snd head)
                                             else encodeChar tail ch

    //
    // encode
    //
    // Usage:       encode table str
    // Description: Encodes a string into a huffman binary string
    //
    // Params       table - the huffman table containing binary encodings of
    //                      ASCII characters
    //              str - The string to encode
    //
    // Returns      A string containing the huffman encoding of a tree
    //
    let encode table str =
        let rec binaryAppend lst =
            match lst with
            | [] -> ""
            | head::tail -> head + binaryAppend tail
        binaryAppend ([for ch in str -> (encodeChar table ch)])

    //
    // decode
    //
    // Usage:       decode tree str
    // Description: Decodes a huffman encoded string into to ASCII string
    //
    // Params       tree - the huffman tree to use in to decoding process
    //              str - The huffman encoded string to decode
    //
    // Returns      the ASCII string decoded from the huffman input string
    //
    let decode tree str=
        let rec recursiveDecoder tree lst retStr =
            match lst with
            | [] ->  match tree with
                     | Leaf(x) -> retStr + new string(x, 1)
                     | _ -> retStr
            | head::tail -> match tree with
                            | Leaf(x) -> recursiveDecoder huffmanTree lst (retStr + new string(x, 1))
                            | Node(left, right) -> match head with
                                                   | '0' ->  recursiveDecoder left tail retStr
                                                   | '1' ->  recursiveDecoder right tail retStr
                                                   | _ -> ""

        recursiveDecoder huffmanTree (explode str) ""




    printfn "Encoding cat: %A\n" (encode huffmanTable "CAT")
    printfn "Ouput: %A" (decode huffmanTree (encode huffmanTable "CAT") )
    printfn "Ouput: %A" (decode huffmanTree (encode huffmanTable "IVE BEEN EVERYWHERE MAN") )
    printfn "Ouput: %A" (decode huffmanTree (encode huffmanTable "ALL THE LOVELY PEOPLE WHERE DO THEY ALL BELONG") )
