#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Hash([u8; 32]);

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

pub fn encode(hashes: Vec<Hash>) -> Palimpsest {
    let length = hashes.len() + 30;
    let mut bytes = vec![0u8; length];
    for hash in 0..hashes.len() {
        for index in 0..32 {
            bytes[hash + index] ^= hashes[hash].0[index];
        }
    }
    Palimpsest(bytes)
}

pub fn decode(palimpsest: Palimpsest, bibliography: Bibliography) -> Option<Vec<Hash>> {
    fn xor(data: &mut Vec<u8>, hash: Hash, offset: usize) {
        for index in 0..data.len() {
            data[index + offset] ^= hash.0[index];
        }
    }

    fn recur(bibliography: &Bibliography, data: &mut Vec<u8>, count: usize, item: usize, hashes: &mut Vec<Hash>) -> Option<Vec<Hash>> {
        if item == count {
            if data.iter().all(|&byte| byte == 0) {
                Some(hashes.clone())
            } else {
                None
            }
        } else {
            bibliography.lookup(data[item]).iter().find_map(|&hash| {
                xor(data, hash, item);
                let result = recur(bibliography, data, count, item + 1, hashes);
                xor(data, hash, item);
                result
            })
        }
    }

    let mut data = palimpsest.bytes();
    let mut hashes: Vec<Hash> = Vec::new();
    let count = palimpsest.bytes().len() - 30;
    recur(&bibliography, &mut data, count, 0, &mut hashes)
}
