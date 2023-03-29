use std::{env,
    fs::File,
    io::{BufReader, Write, BufRead}, 
    cmp::Ordering, collections::HashMap, vec};
use bit_vec::BitVec;
use serde::{Serialize, Deserialize};
use get_size::GetSize;
use std::time::Instant;
use rand::{thread_rng, Rng};


#[derive(Serialize, Deserialize, Debug)] 
struct BitVecSupport {
    bitvect: Option<BitVec>,
}

impl BitVecSupport {

    pub fn save (&self, fname: &str) {
        let bit_struct = bincode::serialize(self).unwrap();

        //this shouldnt be a new file every time soooo instead we need to write to the file with that name (this shoudl apply to all saves)
        //with my new way this might be wrong
        let mut file = File::create(fname).expect("File unable to be created");
        file.write(&bit_struct).expect("File could not be written to");

    }
    
    pub fn load(mut self, fname: &str) {
        //deserialize it 
        let read = BufReader::new(File::open(fname).expect("File unable to be opened"));

        let bit_struct: BitVecSupport =  bincode::deserialize_from(read).expect("Read index in wrong");
 
        self.bitvect = bit_struct.bitvect;
    }
}


//might have to create a specfic struct for the things inside of this. that way they can be serialized and deserialized w/out affecting pointer
#[derive(Serialize, Deserialize, Debug)] 
struct RankSupport {
    cum_rank: Option<Vec<usize>>,
    rel_cum_rank: Option<Vec<usize>>,
    lookup_table: Option<HashMap<usize, HashMap<usize, Vec<usize>>>>,
    bitvec: Option<BitVec>
}

impl RankSupport {
    pub fn rank1(&self, i: usize) -> usize {

        let chunk_len = ((self.bitvec.as_ref().unwrap().len() as f32).log2()).powf(2.0).ceil() as usize;
        let small_chunk_len = (((self.bitvec.as_ref().unwrap().len() as f32).log2()) /2.0).ceil() as usize;

        //get the big chunk it is in
        let big_chunk = i / chunk_len;
        let rank_big_chunk = self.cum_rank.as_ref().unwrap()[big_chunk];

        //get the small chunk it is in  ( i dont think this is right)
        let start_loc_in_small = big_chunk * chunk_len;

        let small_chunk = (i - start_loc_in_small) / small_chunk_len; 
        
        let rank_small_chunk = self.rel_cum_rank.as_ref().unwrap()[small_chunk];

        let spot_in_vec = i - (start_loc_in_small + (small_chunk * small_chunk_len));

        let rank_lookup = self.lookup_table.as_ref().unwrap().get(&big_chunk).unwrap().get(&small_chunk).unwrap()[spot_in_vec];

        //add those together
        return rank_small_chunk + rank_big_chunk + rank_lookup;

    }

    pub fn overhead(&self) -> usize {
        return (self.cum_rank.as_ref().unwrap().get_heap_size() 
            + self.rel_cum_rank.as_ref().unwrap().get_heap_size() 
            + self.lookup_table.as_ref().unwrap().get_heap_size()) * 8;
    }

    pub fn save(&self, fname: &str) {
        //serialize it
        let rank_struct = bincode::serialize(self).unwrap();

        let mut file = File::create(fname).expect("File unable to be created");
        file.write(&rank_struct).expect("File could not be written to");
        

    }

    pub fn load (mut self, fname: &str) {

        //deserialize it 
        let read = BufReader::new(File::open(fname).expect("File unable to be opened"));


        let rank_structure: RankSupport =  bincode::deserialize_from(read).expect("Read index in wrong");

        self.cum_rank = rank_structure.cum_rank;
        self.lookup_table = rank_structure.lookup_table;

        self.rel_cum_rank = rank_structure.rel_cum_rank; 
        self.bitvec = rank_structure.bitvec;  
    }
}

fn build_rank (b: BitVec) -> RankSupport {
    let n: f32 = b.len() as f32;
    let mut rank_counter = 0;
    
    //big chunk variables 
    let chunk_len = (n.log2()).powf(2.0).ceil() as usize;
    let mut cum_rank: Vec<usize> = vec![0];

    //small chunk variables 
    let small_chunk_len = ((n.log2()) /2.0).ceil() as usize;
    let mut rel_cum_rank: Vec<usize> = vec![0];
    let mut small_rank_counter = 0;

    //lookup table variables 
    let mut large_chunk_lookuptables: HashMap<usize, HashMap<usize, Vec<usize>>> = HashMap::new();
    let mut curr_chunk_counter:usize = 0;
    let mut curr_small_chunk: usize = 0;


    for (i, ele) in b.iter().enumerate() {
        // update chunk indices and reset rank
        if i != 0 && i % chunk_len == 0 {
            //handle updating large chunk
            cum_rank.push(rank_counter);
            curr_chunk_counter += 1;
            rank_counter = 0;

            //little chunk
            rel_cum_rank.push(rank_counter);
            curr_small_chunk = 0;
            small_rank_counter = 0;
            
        //start a new small chunk
        } else if i != 0 && i % small_chunk_len == 0 {
           
            //handle what to do with new little chunk
            rel_cum_rank.push(rank_counter);
            small_rank_counter = 0;
            curr_small_chunk += 1;
        }
        
        // after updating the chunk indices, add hashmap value if missing
        if large_chunk_lookuptables.get(&curr_chunk_counter) == None {
            large_chunk_lookuptables.insert(curr_chunk_counter, HashMap::new());
        }

        let little_chunk_lookuptable = large_chunk_lookuptables.get_mut(&curr_chunk_counter).unwrap();
        if little_chunk_lookuptable.get(&curr_small_chunk) == None {
            little_chunk_lookuptable.insert(curr_small_chunk, vec![]);
        }


        // we now DEFINITLY have a hashmap for the little chunk, and DEFINITELY have a vector in that map for hte current small chunk
        // now we can add the rank

        little_chunk_lookuptable.get_mut(&curr_small_chunk).unwrap().push(small_rank_counter);  // add little chunk rank


        if ele == true {
            rank_counter += 1; 
            small_rank_counter += 1;
        }
    }

    let rankstruct = RankSupport {
        cum_rank: Some(cum_rank),
        rel_cum_rank: Some(rel_cum_rank),
        lookup_table: Some(large_chunk_lookuptables),
        bitvec: Some(b),

    };

    return rankstruct;

}

#[derive(Serialize, Deserialize, Debug)]
struct SelectSupport {
    rank: RankSupport, 
}

impl SelectSupport {
    pub fn select1(&self, i: usize) -> Option<usize>{


        let mut l = 0;
        let mut r = self.rank.bitvec.as_ref().unwrap().len(); 
        let mut m;
    
        while l < r {
            m = (r + l) /2;
            let og_m = m;

            while m < self.rank.bitvec.as_ref().unwrap().len() && self.rank.bitvec.as_ref().unwrap()[m] != true {
                if m > r {
                    return None;
                }
                m += 1;
            }

            //"j for which rank1(j) = i"
            //double check that this is the correct comparision 
            match self.rank.rank1(m).cmp(&i) {
                Ordering::Equal => return Some(m),
                Ordering::Greater => r = og_m,
                Ordering::Less => l = og_m + 1
            }
        }
        None
    } 

    pub fn overhead(&self) -> usize {
        return (self.rank.cum_rank.as_ref().unwrap().get_heap_size() 
            + self.rank.rel_cum_rank.as_ref().unwrap().get_heap_size() 
            + self.rank.lookup_table.as_ref().unwrap().get_heap_size()) * 8;
    }

    pub fn save(&self, fname: &str){

        //serialize it
        let select_struct = bincode::serialize(self).unwrap();

        let mut file = File::create(fname).expect("File unable to be created");
        file.write(&select_struct).expect("File could not be written to");

    }
    pub fn load(mut self, fname: &str) {
        let read = BufReader::new(File::open(fname).expect("File unable to be opened"));

        let select_struct: SelectSupport =  bincode::deserialize_from(read).expect("Read index in wrong");

        self.rank = select_struct.rank;

    }

}

#[derive(Serialize, Deserialize, Debug)]
struct Sparse {
    select: Option<SelectSupport>,
    values: Option<HashMap<usize, String>>,
    bitvec: Option<BitVec>,
    finalized: bool,
}

impl Sparse {
    pub fn create(size: usize) -> Self{
        
        let rank = RankSupport {
            cum_rank: None,
            rel_cum_rank: None,
            lookup_table: None,
            bitvec: None
        };
        let select = SelectSupport {
            rank: rank,
        };

        return Sparse{
            select: Some(select),
            values: Some(HashMap::new()),
            bitvec: Some(BitVec::from_elem(size, false)),
            finalized: false
        };
    }

    pub fn append(&mut self, elem: String, pos: usize) {
        if self.finalized == false {
            if pos >= self.bitvec.as_ref().unwrap().len() {
                //error here
            } else {
                self.bitvec.as_mut().unwrap().set(pos, true);
                self.values.as_mut().unwrap().insert(pos, elem);
            }
        }
    }

    pub fn finalize(&mut self) {
        self.finalized = true;
        let b = self.bitvec.as_ref().unwrap().clone();
        self.select.as_mut().unwrap().rank = build_rank(b);
    }

    //either str or String (and the one below)
    pub fn get_at_rank(&self, r: usize, elem: &mut String) -> bool {
        if self.values.as_ref().unwrap().get(&self.select.as_ref().unwrap().select1(r).unwrap()) == None {
            return false;
        } else {
            *elem = self.values.as_ref().unwrap().get(&self.select.as_ref().unwrap().select1(r).unwrap()).unwrap().clone();
            return true;
        }
        

    }

    pub fn get_at_index(&self, r: usize, elem: &mut String) -> bool {
        if  self.bitvec.as_ref().unwrap()[r] == true {
            *elem = self.values.as_ref().unwrap().get(&r).unwrap().clone();
            return true;
        } else {
            return false;
        }
    }  

    //check on this one
    pub fn get_index_of(&self, r: usize) -> usize {
        if r >= self.values.as_ref().unwrap().len() {
            //return a sentianl value
            return 7;
        } else {
            if self.select.as_ref().unwrap().select1(r-1) == None {
                return 1;
            } else  {
                return self.select.as_ref().unwrap().select1(r-1).unwrap();
            }
            
        }
    }

    pub fn num_elem_at(&self, r: usize) -> usize{
        return self.select.as_ref().unwrap().rank.rank1(r+1);
    }

    pub fn size(&self) -> usize {
        return self.bitvec.as_ref().unwrap().len();
    }

    pub fn num_elem(&self) -> usize {
        return self.values.as_ref().unwrap().len();
    }
    pub fn save(&self, fname: &String) {
        //serialize it
        let sparse_struct = bincode::serialize(self).unwrap();

        let mut file = File::create(fname).expect("File unable to be created");
        file.write(&sparse_struct).expect("File could not be written to");
    }

    pub fn load(mut self, fname: &String) {
        let read = BufReader::new(File::open(fname).expect("File unable to be opened"));

        let sparse_struct: Sparse =  bincode::deserialize_from(read).expect("Read index in wrong");

        self.bitvec = sparse_struct.bitvec;
        self.values = sparse_struct.values;
        self.select = sparse_struct.select;
    }
}

fn main() {
    let args:Vec<String> = env::args().collect();

    if args.len() != 2 {
        println!("too many args or too fewer args, needs to have 2");
        return;
    }

    let read = BufReader::new(File::open(&args[1]).expect("File unable to be opened"));
    let mut b = BitVec::new();

    for line_result in read.lines() {
        for c in line_result.unwrap().chars(){
            if c == '1' {
                b.push(true);
            } else if c == '0' {
                b.push(false);
            } else {
                continue;
            }
        }


    }

    let rank = build_rank(b);

    let select = SelectSupport {
        rank: rank,
    };

    


    let rank_time = Instant::now();
    select.select1(2);
    select.select1(12);
    select.select1(23);
    select.select1(32);
    select.select1(47);
    select.select1(59);
    select.select1(69);
    select.select1(78);

    let duration = rank_time.elapsed();
   // println!("Time of 8 rank queries: {:?}", duration);
    //print!("{:?}", select.overhead());
 


}