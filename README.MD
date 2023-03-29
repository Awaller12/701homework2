To start using this program you need to create a file that has a string or chars that are either 1's or 0's, any other character will be ignored. Then run this file on the command line using "cargo run filename". Add any functions you want to call to main.

# BitVector<div style="text-align:center">

TALK ABOUT FILE TYPE FOR THE LOAD

This part of the code contains the creation of the Bitvector and the RankStructure. The BitVector structure only requires a Option<BitVec> to be placed into it. Meaning it can contain None or Some(BitVec). This structure can be implemented with two functions, save and load. These functions can be called by using creating a BitVector, lets say it is named bitvec, and calling bitvec.save("filename") or bitvec.load("filename"). This allows for the strucuture to be be serialzied and saved to a file name provided for save. Load does required a structure to be passed with it but this structure can be completely initalized to None. When the information is passed in pulled from the file it will be set equal to each part of the structure. This will be the case for all the loads and saves. 

# RankStructure<div style="text-align:center">

The second part of the code that is created in this seciton is the RankStructure. This is a structure and a class. This Strucutre contains cum_rank: Option<Vec<usize>>, rel_cum_rank: Option<Vec<usize>>, lookup_table: Option<HashMap<usize, HashMap<usize, Vec<usize>>>>, bitvec: Option<BitVec> which respectivaly represent the ranks of all chunks for a bitvector of size log2(n)^2, with in being the length of the bitvector, the ranks of all the smaller chunks from a bitvector of size log2(n)/2, a table containing the exact rank of each index (dependent on the the smaller chunks), and a bitvector where all the information listed prior will be pulled from. The goal of this structure is to help bulid and use a rank1 function in the time of O(1) and space of o(n). The goal of this function is to understand how many 1's are at a specfic index of a bitvector, exclusively. 

Along with save and load, which are descripted in the Bitvector seciton, this class has two functions, rank1 and overhead. Rank1 is discussed above but to use it you have to call build_rank() on your intended bitvector and set it equal to a new RankStrucuture, as the function will create a new one. This RankStructure can then be used to call Rank1 or Overhead on. Rank1 requires an index to be passed in with it. Overhead does not need any inputs but will tell you about the size of the rank data structure in bits. 


# SelectStructure<div style="text-align:center">

SelectStructure contains a RankStructure. This is because all ascpets of Select can be pulled from different pieces of the RankStructure. Due to this you must first create a RankStructure and then you can initialzie a SelectStructure with that. Along with the Class above SelectStructure has a load and save, which you can learn more about in the Bitvector section. In a similar way this Structure allows for the implementation of a overhead function, which is explained in RankStructure section. The unique function to this Class is the select1 function. This function requires an rank to be passed in. It will then return the position at which this rank first occurs. It would be called in a similar way to the above functions by creating a SelectStructure and then doing select.select1(5). 

# SparseStructure<div style="text-align:center">

The goal of Sparse is to have a bitvector that has 1's at corresponding spots to where values, that are stored in another list, are located. Sparse is a more unique structure compared to the other structures listed above. The Sparse structure requires select: Option<SelectSupport>, values: Option<HashMap<usize, String>>, bitvec: Option<BitVec>, and finalized: bool. Having a Select structure allows for the functions from both select and rank to be used. Values holds the values that are represented in the bitvector and are linked, by the hashamp, to values that are their positions in the bitvector. The finalized boolean is used to ensure that the bitvector or values is written to once it has been "finalized". 

Sparse vector contains 11 different functions that can be used in called. They are listed below as well as a short description of what they do. To call these functions you much create a SparseStructure. Lets say an example one is called sparse. Then you would call sparse.nameofthefunction() and adding what ever required parameters to the parenthese. 

- create(size: usize): Creates an empty sparse array size of bitvector you want to make

- append(elem string, pos usize): Adds an element elem to the given pos of the sparse array

- finalize(): This makes the sparse array unmutable

- get_at_rank(r: usize, elem: &String): Places a reference to r-th item at the reference elm in the array. Returns true if it was there and false otherwise.

- get_at_index(r: usize, elem: &String): Goes to the rth index of the bitvector, if 1 binds the value to elem and returns true, else returns false

- get_index_of(r: usize) : Returns the index of the sparse array at the r-th element based on the rank r

- num_elem_at(r: usize): Returns the number of elements up to and at index r

 - size(): Returns the size of the sparse array.

- num_elem(): Returns the number of 1's in the bitvector

- save(fname): Uses the filename and saves the sparse array there.

- load(fname): Uses the filename and loads in a sparse array from there.