pub type Hash = [u8; 32];

#[derive(Debug, Clone, PartialEq)]
pub struct Palimpsest(Vec<u8>);

#[derive(Debug, Clone, PartialEq)]
pub struct Bibliography([Vec<Hash>; 256]);

impl Bibliography {
    fn new() -> Self {
        Self(std::array::from_fn(|_| Vec::new()))
    }

    pub fn lookup(&self, key: u8) -> Vec<Hash> {
        self.0[key as usize].clone()
    }
}

impl Palimpsest {
    pub fn bytes(&self) -> Vec<u8> {
        self.0.clone()
    }
}
pub fn encode(hashes: Vec<&Hash>) -> Palimpsest {
    let length = hashes.len() + 30;
    let mut bytes = vec![0u8; length];
    for hash in 0..hashes.len() {
        for index in 0..32 {
            bytes[hash + index] ^= hashes[hash][index];
        }
    }
    Palimpsest(bytes)
}

pub fn decode(palimpsest: Palimpsest, bibliography: Bibliography) -> Vec<Hash> {
    let count = palimpsest.bytes().len() - 30;
    let mut array = palimpsest.bytes();

    fn xor(mut data: Vec<u8>, offset: usize) {
        for index in 0..data.len() {
            data[index + offset] ^= data[index];
        }
    }

    fn complete(data: Vec<u8>) -> bool {
        data.iter().all(|&byte| byte == 0)
    }

    fn recur(item: usize, hashes: Vec<Hash>) -> Option<Vec<Hash>> {
        if item == length {
            if complete(&hashes) {
                Some(hashes)
            } else {
                None
            }
        } else {
            bibliography.lookup(array(item)).for_each(|hash| {
                xor(hash, item as usize);
                match recur(item + 1, hashes.push(hash)) {
                    Some(result) => Some(result)
                    None =>
                      xor(hash, item as usize);
                      continue
                }

            });
        }
    }

    recur(0, Vec::new())
}
