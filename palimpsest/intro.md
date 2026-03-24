Given a server holding a potentially large number of chunks of data, and a client holding a subset of that data, the client may want to specify an ordered sequence of data chunks to the server.

New chunks of data may be added to the server without the client knowing, but must never be removed.

Naively, the client could transmit all the data to the server, and the server would then know the sequence of data. But this would be highly inefficient.

A much better way would be for the server and client to calculate a cryptographic hash of every chunk of data, and to send that sequence to the server. The server could resolve each hash to each chunk of data to establish the order.

For SHA-256 hashes, this would require sending `32*n` bytes of data, where `n` is the number of chunks of data.

But it's possible to do better, at the expense of potentially requiring more computation by the server. It is possible to send the same information in `32 + k(n - 1)` bytes, where `k` is proportional to the log of the number of chunks of data the server knows.

As an example, if the server contained about 10 million chunks of data, k should be chosen to be 3, and a subsequence of length 100 could be sent in 329 bytes rather than 3200 bytes, almost ten times less. If the server only contained 1000 records, k could be chosen to be 1 and a subsequence of 100 could be sent in 131 bytes, an improvement of nearly 25.

This method relies on combining the hashes in a _palimpsest_. For a given choice of `k`, this is formed by taking each hash with distinct index `i` from `0` upto `n`, prefixing it with `k*i` zero bytes and suffixing it with `k*(n - 1 - i)` zero bytes. These hashes are then XORed together (in any order) to produce a single array of length `32 + k(n - 1)`. This is the palimpsest for that sequence of hashes. It can be decoded by any system with access to a superset of the hashes it contains.

To decode a palimpsest on a server, the remote needs its library of hashes to be indexed by their first k bytes. The algorithm is defined recursively:
1. if the palimpsest is exactly 32 bytes long, and every byte is zero, then return the stack of hashes collected so far, otherwise,
2. find all the hashes whose first k bytes match the first k bytes of the palimpsest. If there are none, pop the hash and try the next one
3. for each matching hash, XOR the first 32 bytes of the palimpsest with the hash
4. the first k bytes will be zero; remove them and recurse

Decoding appears to be an NP-hard problem. For a library of `n` hashes, these must fit into `2^k` distinct buckets. For now, let's assume the hashes are perfectly uniformly distributed throughout the buckets. The algorithm will do "wasted work" if, in addition to the correct hash, it finds other hashes which also match the first n bytes, but which lead to a dead end.

In general, the hashes will NOT be uniformly distributed across the buckets. If one bucket contains more than one hash, that implies that the mean number of hashes in the other (k - 1) buckets must be less.

For around 256 chunks, `k` should be around 1. For 65536 chunks, 2 is sufficient. For 16.7 million chunks, 3 is sufficient. For 4.3 billion chunks, 4 is sufficient.

(n/(2^k))*log(s)
