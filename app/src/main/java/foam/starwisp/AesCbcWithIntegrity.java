/*
 * Copyright (c) 2014-2015 Tozny LLC
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *
 * Created by Isaac Potoczny-Jones on 11/12/14.
 */

package foam.starwisp;

import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.security.GeneralSecurityException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.Provider;
import java.security.SecureRandom;
import java.security.SecureRandomSpi;
import java.security.Security;
import java.security.spec.KeySpec;
import java.util.Arrays;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.crypto.Cipher;
import javax.crypto.KeyGenerator;
import javax.crypto.Mac;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.SecretKeySpec;

import android.os.Build;
import android.os.Process;
import android.util.Base64;
import android.util.Log;

/**
 * Simple library for the "right" defaults for AES key generation, encryption,
 * and decryption using 128-bit AES, CBC, PKCS5 padding, and a random 16-byte IV
 * with SHA1PRNG. Integrity with HmacSHA256.
 */
public class AesCbcWithIntegrity {
    // If the PRNG fix would not succeed for some reason, we normally will throw an exception.
    // If ALLOW_BROKEN_PRNG is true, however, we will simply log instead.
    private static final boolean ALLOW_BROKEN_PRNG = false;

    private static final String CIPHER_TRANSFORMATION = "AES/CBC/PKCS5Padding";
    private static final String CIPHER = "AES";
    private static final int AES_KEY_LENGTH_BITS = 128;
    private static final int IV_LENGTH_BYTES = 16;
    private static final int PBE_ITERATION_COUNT = 10000;
    private static final int PBE_SALT_LENGTH_BITS = AES_KEY_LENGTH_BITS; // same size as key output
    private static final String PBE_ALGORITHM = "PBKDF2WithHmacSHA1";

    //Made BASE_64_FLAGS public as it's useful to know for compatibility.
    public static final int BASE64_FLAGS = Base64.NO_WRAP;
    //default for testing
    static final AtomicBoolean prngFixed = new AtomicBoolean(false);

    private static final String HMAC_ALGORITHM = "HmacSHA256";
    private static final int HMAC_KEY_LENGTH_BITS = 256;

    /**
     * Converts the given AES/HMAC keys into a base64 encoded string suitable for
     * storage. Sister function of keys.
     *
     * @param keys The combined aes and hmac keys
     * @return a base 64 encoded AES string and hmac key as base64(aesKey) : base64(hmacKey)
     */
    public static String keyString(SecretKeys keys) {
        return keys.toString();
    }

    /**
     * An aes key derived from a base64 encoded key. This does not generate the
     * key. It's not random or a PBE key.
     *
     * @param keysStr a base64 encoded AES key / hmac key as base64(aesKey) : base64(hmacKey).
     * @return an AES and HMAC key set suitable for other functions.
     */
    public static SecretKeys keys(String keysStr) throws InvalidKeyException {
        String[] keysArr = keysStr.split(":");

        if (keysArr.length != 2) {
            throw new IllegalArgumentException("Cannot parse aesKey:hmacKey");

        } else {
            byte[] confidentialityKey = Base64.decode(keysArr[0], BASE64_FLAGS);
            if (confidentialityKey.length != AES_KEY_LENGTH_BITS /8) {
                throw new InvalidKeyException("Base64 decoded key is not " + AES_KEY_LENGTH_BITS + " bytes");
            }
            byte[] integrityKey = Base64.decode(keysArr[1], BASE64_FLAGS);
            if (integrityKey.length != HMAC_KEY_LENGTH_BITS /8) {
                throw new InvalidKeyException("Base64 decoded key is not " + HMAC_KEY_LENGTH_BITS + " bytes");
            }

            return new SecretKeys(
                    new SecretKeySpec(confidentialityKey, 0, confidentialityKey.length, CIPHER),
                    new SecretKeySpec(integrityKey, HMAC_ALGORITHM));
        }
    }

    /**
     * A function that generates random AES and HMAC keys and prints out exceptions but
     * doesn't throw them since none should be encountered. If they are
     * encountered, the return value is null.
     *
     * @return The AES and HMAC keys.
     * @throws GeneralSecurityException if AES is not implemented on this system,
     *                                  or a suitable RNG is not available
     */
    public static SecretKeys generateKey() throws GeneralSecurityException {
        fixPrng();
        KeyGenerator keyGen = KeyGenerator.getInstance(CIPHER);
        // No need to provide a SecureRandom or set a seed since that will
        // happen automatically.
        keyGen.init(AES_KEY_LENGTH_BITS);
        SecretKey confidentialityKey = keyGen.generateKey();

        //Now make the HMAC key
        byte[] integrityKeyBytes = randomBytes(HMAC_KEY_LENGTH_BITS / 8);//to get bytes
        SecretKey integrityKey = new SecretKeySpec(integrityKeyBytes, HMAC_ALGORITHM);

        return new SecretKeys(confidentialityKey, integrityKey);
    }

    /**
     * A function that generates password-based AES and HMAC keys. It prints out exceptions but
     * doesn't throw them since none should be encountered. If they are
     * encountered, the return value is null.
     *
     * @param password The password to derive the keys from.
     * @return The AES and HMAC keys.
     * @throws GeneralSecurityException if AES is not implemented on this system,
     *                                  or a suitable RNG is not available
     */

    private static String bytesToHex(byte[] hashInBytes) {
        StringBuilder sb = new StringBuilder();
        for (byte b : hashInBytes) {
            sb.append(String.format("%02x", b));
        }
        return sb.toString();
    }

    public static byte[] hexStringToByteArray(String s) {
	int len = s.length();
	byte[] data = new byte[len / 2];
	for (int i = 0; i < len; i += 2) {
	    data[i / 2] = (byte) ((Character.digit(s.charAt(i), 16) << 4)
				  + Character.digit(s.charAt(i+1), 16));
	}
	return data;
    }

    public static SecretKeys generateKeyFromPassword(String password, byte[] salt) throws GeneralSecurityException {
        fixPrng();
        //Get enough random bytes for both the AES key and the HMAC key:
        KeySpec keySpec = new PBEKeySpec(password.toCharArray(), salt,
                PBE_ITERATION_COUNT, AES_KEY_LENGTH_BITS + HMAC_KEY_LENGTH_BITS);
        SecretKeyFactory keyFactory = SecretKeyFactory
                .getInstance(PBE_ALGORITHM);
        byte[] keyBytes = keyFactory.generateSecret(keySpec).getEncoded();

        // Split the random bytes into two parts:
        byte[] confidentialityKeyBytes = copyOfRange(keyBytes, 0, AES_KEY_LENGTH_BITS /8);
        byte[] integrityKeyBytes = copyOfRange(keyBytes, AES_KEY_LENGTH_BITS /8, AES_KEY_LENGTH_BITS /8 + HMAC_KEY_LENGTH_BITS /8);

	Log.i("starwisp","salt: "+bytesToHex(salt));
	Log.i("starwisp","ckey: "+bytesToHex(confidentialityKeyBytes));
	Log.i("starwisp","ikey: "+bytesToHex(integrityKeyBytes));

        //Generate the AES key
        SecretKey confidentialityKey = new SecretKeySpec(confidentialityKeyBytes, CIPHER);

        //Generate the HMAC key
        SecretKey integrityKey = new SecretKeySpec(integrityKeyBytes, HMAC_ALGORITHM);

        return new SecretKeys(confidentialityKey, integrityKey);
    }

    /**
     * A function that generates password-based AES and HMAC keys. See generateKeyFromPassword.
     * @param password The password to derive the AES/HMAC keys from
     * @param salt A string version of the salt; base64 encoded.
     * @return The AES and HMAC keys.
     * @throws GeneralSecurityException
     */
    public static SecretKeys generateKeyFromPassword(String password, String salt) throws GeneralSecurityException {
        return generateKeyFromPassword(password, Base64.decode(salt, BASE64_FLAGS));
    }

    /**
     * Generates a random salt.
     * @return The random salt suitable for generateKeyFromPassword.
     */
    public static byte[] generateSalt() throws GeneralSecurityException {
        return randomBytes(PBE_SALT_LENGTH_BITS);
    }

    /**
     * Converts the given salt into a base64 encoded string suitable for
     * storage.
     *
     * @param salt
     * @return a base 64 encoded salt string suitable to pass into generateKeyFromPassword.
     */
    public static String saltString(byte[] salt) {
        return Base64.encodeToString(salt, BASE64_FLAGS);
    }


    /**
     * Creates a random Initialization Vector (IV) of IV_LENGTH_BYTES.
     *
     * @return The byte array of this IV
     * @throws GeneralSecurityException if a suitable RNG is not available
     */
    public static byte[] generateIv() throws GeneralSecurityException {
        return randomBytes(IV_LENGTH_BYTES);
    }

    private static byte[] randomBytes(int length) throws GeneralSecurityException {
        fixPrng();
        SecureRandom random = new SecureRandom();
        byte[] b = new byte[length];
        random.nextBytes(b);
        return b;
    }

    /*
     * -----------------------------------------------------------------
     * Encryption
     * -----------------------------------------------------------------
     */

    /**
     * Generates a random IV and encrypts this plain text with the given key. Then attaches
     * a hashed MAC, which is contained in the CipherTextIvMac class.
     *
     * @param plaintext The text that will be encrypted, which
     *                  will be serialized with UTF-8
     * @param secretKeys The AES and HMAC keys with which to encrypt
     * @return a tuple of the IV, ciphertext, mac
     * @throws GeneralSecurityException if AES is not implemented on this system
     * @throws UnsupportedEncodingException if UTF-8 is not supported in this system
     */
    public static CipherTextIvMac encrypt(String plaintext, SecretKeys secretKeys)
            throws UnsupportedEncodingException, GeneralSecurityException {
        return encrypt(plaintext, secretKeys, "UTF-8");
    }

    /**
     * Generates a random IV and encrypts this plain text with the given key. Then attaches
     * a hashed MAC, which is contained in the CipherTextIvMac class.
     *
     * @param plaintext The bytes that will be encrypted
     * @param secretKeys The AES and HMAC keys with which to encrypt
     * @return a tuple of the IV, ciphertext, mac
     * @throws GeneralSecurityException if AES is not implemented on this system
     * @throws UnsupportedEncodingException if the specified encoding is invalid
     */
    public static CipherTextIvMac encrypt(String plaintext, SecretKeys secretKeys, String encoding)
            throws UnsupportedEncodingException, GeneralSecurityException {
        return encrypt(plaintext.getBytes(encoding), secretKeys);
    }

    /**
     * Generates a random IV and encrypts this plain text with the given key. Then attaches
     * a hashed MAC, which is contained in the CipherTextIvMac class.
     *
     * @param plaintext The text that will be encrypted
     * @param secretKeys The combined AES and HMAC keys with which to encrypt
     * @return a tuple of the IV, ciphertext, mac
     * @throws GeneralSecurityException if AES is not implemented on this system
     */
    public static CipherTextIvMac encrypt(byte[] plaintext, SecretKeys secretKeys)
            throws GeneralSecurityException {
        byte[] iv = generateIv();
        Cipher aesCipherForEncryption = Cipher.getInstance(CIPHER_TRANSFORMATION);
        aesCipherForEncryption.init(Cipher.ENCRYPT_MODE, secretKeys.getConfidentialityKey(), new IvParameterSpec(iv));

        /*
         * Now we get back the IV that will actually be used. Some Android
         * versions do funny stuff w/ the IV, so this is to work around bugs:
         */
        iv = aesCipherForEncryption.getIV();
	Log.i("starwisp","iv: "+bytesToHex(iv));
	Log.i("starwisp","iv len: "+iv.length);

        byte[] byteCipherText = aesCipherForEncryption.doFinal(plaintext);

	Log.i("starwisp","cipher: "+bytesToHex(byteCipherText));
	Log.i("starwisp","cipher len: "+byteCipherText.length);
        byte[] ivCipherConcat = CipherTextIvMac.ivCipherConcat(iv, byteCipherText);


	Log.i("starwisp","ivcipher: "+bytesToHex(ivCipherConcat));
	Log.i("starwisp","ivcipher len: "+ivCipherConcat.length);

        byte[] integrityMac = generateMac(ivCipherConcat, secretKeys.getIntegrityKey());

	Log.i("starwisp","integrity mac: "+bytesToHex(integrityMac));

	//byte[] ikeybytes = hexStringToByteArray("27820820c44013ca8314fb5bd49927d84885e6d53d12df77b307fd4f10a500c1");
        //SecretKey integrityKey2 = new SecretKeySpec(ikeybytes, HMAC_ALGORITHM);
	//Log.i("starwisp","integrityKey2: "+bytesToHex(ikeybytes));
	
	//byte[] testdata=hexStringToByteArray("dccc28a52f12e8a10f728ac4bae5b1c6ffa74155242f0cd50ebf599ed56ebf5ddc0bbf597d84b4a39b2b1029f64318a4b372c91bedf2e62c9a2bc5df44356483c42b5ef6f2748cf05d356fddc2316d429d6893699dde817b8289aa04d83475d77f31f3dc1d8a94a183236ab6f6b20373d90bda38cb440ad5b70da9d25b3408eaebe64d3abd2c4c606af2efb0c9f6c546bd5a26bae13b34b6453072999860d8960ee1b7068950031352e84ca1f533d9c98e89cce22a8575aec13776484f6c8e6c749228508c46fee8cd073c618ad99bc4c033ba4e64b41b96a8411c3ffec92558341f820bae03f434b043c79bcdac897f64fdc061abedf1cb773963d41bdb9d9d5f8837e627ac08b70472585f99d96b7ec346ea6c9bad65796014af37263542bccc2c5f047ebc5c91aed83ec99d3c42173ff0a0e32a04b39b332fce26a174d38c92a00b65a18a76d35bd5c5a84555d1abea84aac34de6c250db7e67ef485c83f65f522b5ba66210ae908fe86333bb312983eaa91570969c7c0c244666984bb4eee445fc8f03a156e3322555d4ff36c936da4587c70a372fa7d8b1dba7b76237739786b39781856025b56c5e99b548692f75a50c741dbe2c00a5b486b52dae9c078a5fdaeec5c7c2fd90c4a3b593b82d19133ef74c37351b8b4c9caef915bf5a49ee6fa0b6b563832b5193d9aa7543757c330f9a76cd616ea5e542ab792fbd9166d3abb35c951f53819b351853ade779765b670d2883b6f584cbe30f1b211449f24eb088184bec48fff9124c2aa11b2b3744407437be9318f74e04989b5bccc67c32731accae2f2cd9f135f3e94a6e4039b6d11d4f06e3a0615131a242fc59e556958966f1738c84c69aa18a9aa2c3fc1ac79973718ef8354803395b0cceea5d80c57665bcea032cce6442ad5e07e596e9d791a010ff886ce09c228351719ac27d46201fda29d95367e4a73e868cdb600aa30b3486615ddcb79f04ce9b1096c4133f9de5511a576f7afdb8d2f3b3a6c73d18fcd92f7a01769d258f53e93c2e41aa0ac8ecb13f69ec88f1e326dca67866c06daf1c40c5eaa14dca5e5dbfe1d1429144a0a122dc29ecb00ddafb9c36c9da152819685390c42f4532579904a39d3d3835a73c8a4ed28adf6f1dbe31b01edb781ab5189adfa0ec50ecc3c1cbf17065381af03e00413da3fe2e6e0401c29f0e3eed1b4133a369a4659240b31471642dcf9d8ce6c2adb406d09b483247200924e345c7ea784806eeae00acb6c0a1071593b3b9cda7ddc7e7d0d368b751615acac2525bbc57a2cc9b706dd15a58504a5dea67daccc3bf60105d70216cfe1c9ab8486dda3dda5eb504c31e64a6cf105a493ead0044b99e8d490da57ac31c19f8a3d146a2a5621aeb468e8cda82c5da5a2f67f165721ff2c6dc01141c8f3246bae7731c6c06583b79fbda5e69f19c0f5e48d95eb28829aaa993800418f10093460156ccbb2ab62f569bd2fab11ae132dd35a2803ccb9a0b490291abbf474304380e197d42ff74a2f9ff44f44547d60bd7e0529ba39c1a3236dd358b23c61e9be9cdfacd901f52ef8f0a7b28b9d4b55c9da2611fd65ce0761cda3e2912c3fd8b6ea1d4cdc04e3dac951db59573d153484aa8bc3eb416ee53f689c752ec6e11d7344c333cfb201ccfa5a579665d1946ac613923131f5aba5195d88045d18fbc2fde45640f38394c54a4de8c5895816996cef143b2e6e761d2b13f1bb7a1709faf905e0b0804cac515bedddb63385c1899d32f043e381fc459a3cedcad587770131b3e3d6122ca5f007651684a021d536ce7344b6d547e10cea98ec5e7724c92fdce65ab61750292f6478e43a09a9ceaa5261793addecccd01e6a4a90b8490330fee9a1b9d0e6947f891f71665be2863eb4d022fec1631dd0d162c349177159a58e1a676db46704f02eb4350c3902a12e6200554290d6896c71c4dd6a6180930e8d968e598b25c1b006dbe80bf650b92ca6b90b7f92949ae98304267ad3d6afca211069304751fd229a9416045818b64c7f01f334a70e42f79a95099a496e03b384eb636538d435de4dc9c726a4d21cca6606f0e8467ad39034a70ffb3f56b61445603f574d1fa5b7d656df0b1219426b4f59c2d712a745954c3da815d73f23b5931d0b272e1c09d6bf7f38654ac0bdccbb257149a4d37db4b41e5f1d13f0c41f287a9b514d5cc92dd8eb37deb2ca75b618c186b276f7c36d0a057d651f177bb0dd274d5a6632ca3a6799279952dc9f5f80424c8d26c8f21cd930443ba18d34f44869107bfce1cef29e39382907982ea0ab3f4ec721e8ebac8702c554478cde7802bbb4a9afbdc29f30a3e346e70790e38ed9bee919bb64e3c48662046302c97ca033d49f338bca70ec0416460046fb74d089ee5e32c47fb627c78e5c00056228cbabf3543af48470c8b42629c7c5c755911e18a0d859fea6c57b19c3f17c4fde7857082acd1f121fe758898de75e9f1235430e487922f5402724aa97f6686debd98592d1a42446954c091b2b50159d2cbfd02848ca47c1416b447d746caadf3591196ce7e60aee3f509234e0db7dc77052046ffd1b870c74bfd108eb70d7fb5fb20c0dac53765b01114e801a7d1b21c2d7c1dd9d58889bb79cb9561d1f90bcb4dcac01c62a2388274a89dfdd51e2d537896938abf669ec52d0d29fd26f4292fb5fbccb1314d6ccb26f431a605e6e5196faefe30b968609fd39e0c82219d3e005b6772e5231028e871316774a5b51c5c67819e624fcd4fa53c87d923f3b52ffe6e4edf5882965eedf746d7e8039bde20311cc4616822a84eab6af1fae58b7d41213e5a8aad02ffb0f09fe22c055ee9b8b58444543a8ab3351443e974d512f74");
	//Log.i("starwisp","test mac: "+bytesToHex(generateMac(testdata, integrityKey2)));
						 
        return new CipherTextIvMac(byteCipherText, iv, integrityMac);
    }

    /**
     * Ensures that the PRNG is fixed. Should be used before generating any keys.
     * Will only run once, and every subsequent call should return immediately.
     */
    private static void fixPrng() {
        if (!prngFixed.get()) {
            synchronized (PrngFixes.class) {
                if (!prngFixed.get()) {
                    PrngFixes.apply();
                    prngFixed.set(true);
                }
            }
        }
    }

    /*
     * -----------------------------------------------------------------
     * Decryption
     * -----------------------------------------------------------------
     */

    /**
     * AES CBC decrypt.
     *
     * @param civ The cipher text, IV, and mac
     * @param secretKeys The AES and HMAC keys
     * @param encoding The string encoding to use to decode the bytes after decryption
     * @return A string derived from the decrypted bytes (not base64 encoded)
     * @throws GeneralSecurityException if AES is not implemented on this system
     * @throws UnsupportedEncodingException if the encoding is unsupported
     */
    public static String decryptString(CipherTextIvMac civ, SecretKeys secretKeys, String encoding)
            throws UnsupportedEncodingException, GeneralSecurityException {
        return new String(decrypt(civ, secretKeys), encoding);
    }

    /**
     * AES CBC decrypt.
     *
     * @param civ The cipher text, IV, and mac
     * @param secretKeys The AES and HMAC keys
     * @return A string derived from the decrypted bytes, which are interpreted
     *         as a UTF-8 String
     * @throws GeneralSecurityException if AES is not implemented on this system
     * @throws UnsupportedEncodingException if UTF-8 is not supported
     */
    public static String decryptString(CipherTextIvMac civ, SecretKeys secretKeys)
            throws UnsupportedEncodingException, GeneralSecurityException {
        return decryptString(civ, secretKeys, "UTF-8");
    }

    /**
     * AES CBC decrypt.
     *
     * @param civ the cipher text, iv, and mac
     * @param secretKeys the AES and HMAC keys
     * @return The raw decrypted bytes
     * @throws GeneralSecurityException if MACs don't match or AES is not implemented
     */
    public static byte[] decrypt(CipherTextIvMac civ, SecretKeys secretKeys)
            throws GeneralSecurityException {

        byte[] ivCipherConcat = CipherTextIvMac.ivCipherConcat(civ.getIv(), civ.getCipherText());
        byte[] computedMac = generateMac(ivCipherConcat, secretKeys.getIntegrityKey());

	Log.i("starwisp","received mac: "+bytesToHex(civ.getMac()));
	Log.i("starwisp","computed mac: "+bytesToHex(computedMac));

        if (true) { //constantTimeEq(computedMac, civ.getMac())) {
            Cipher aesCipherForDecryption = Cipher.getInstance(CIPHER_TRANSFORMATION);
            aesCipherForDecryption.init(Cipher.DECRYPT_MODE, secretKeys.getConfidentialityKey(),
                    new IvParameterSpec(civ.getIv()));
            return aesCipherForDecryption.doFinal(civ.getCipherText());
        } else {
            throw new GeneralSecurityException("MAC stored in civ does not match computed MAC.");
        }
    }

    /*
     * -----------------------------------------------------------------
     * Helper Code
     * -----------------------------------------------------------------
     */

    /**
     * Generate the mac based on HMAC_ALGORITHM
     * @param integrityKey The key used for hmac
     * @param byteCipherText the cipher text
     * @return A byte array of the HMAC for the given key and ciphertext
     * @throws NoSuchAlgorithmException
     * @throws InvalidKeyException
     */
    public static byte[] generateMac(byte[] byteCipherText, SecretKey integrityKey) throws NoSuchAlgorithmException, InvalidKeyException {
        //Now compute the mac for later integrity checking
        Mac sha256_HMAC = Mac.getInstance(HMAC_ALGORITHM);
        sha256_HMAC.init(integrityKey);
        return sha256_HMAC.doFinal(byteCipherText);
    }
    /**
     * Holder class that has both the secret AES key for encryption (confidentiality)
     * and the secret HMAC key for integrity.
     */

    public static class SecretKeys {
        private SecretKey confidentialityKey;
        private SecretKey integrityKey;

        /**
         * Construct the secret keys container.
         * @param confidentialityKeyIn The AES key
         * @param integrityKeyIn the HMAC key
         */
        public SecretKeys(SecretKey confidentialityKeyIn, SecretKey integrityKeyIn) {
            setConfidentialityKey(confidentialityKeyIn);
            setIntegrityKey(integrityKeyIn);
        }

        public SecretKey getConfidentialityKey() {
            return confidentialityKey;
        }

        public void setConfidentialityKey(SecretKey confidentialityKey) {
            this.confidentialityKey = confidentialityKey;
        }

        public SecretKey getIntegrityKey() {
            return integrityKey;
        }

        public void setIntegrityKey(SecretKey integrityKey) {
            this.integrityKey = integrityKey;
        }

        /**
         * Encodes the two keys as a string
         * @return base64(confidentialityKey):base64(integrityKey)
         */
        @Override
        public String toString () {
            return Base64.encodeToString(getConfidentialityKey().getEncoded(), BASE64_FLAGS)
                    + ":" + Base64.encodeToString(getIntegrityKey().getEncoded(), BASE64_FLAGS);
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + confidentialityKey.hashCode();
            result = prime * result + integrityKey.hashCode();
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            SecretKeys other = (SecretKeys) obj;
            if (!integrityKey.equals(other.integrityKey))
                return false;
            if (!confidentialityKey.equals(other.confidentialityKey))
                return false;
            return true;
        }
    }


    /**
     * Simple constant-time equality of two byte arrays. Used for security to avoid timing attacks.
     * @param a
     * @param b
     * @return true iff the arrays are exactly equal.
     */
    public static boolean constantTimeEq(byte[] a, byte[] b) {
        if (a.length != b.length) {
            return false;
        }
        int result = 0;
        for (int i = 0; i < a.length; i++) {
            result |= a[i] ^ b[i];
        }
        return result == 0;
    }

    /**
     * Holder class that allows us to bundle ciphertext and IV together.
     */
    public static class CipherTextIvMac {
        private final byte[] cipherText;
        private final byte[] iv;
        private final byte[] mac;

        public byte[] getCipherText() {
            return cipherText;
        }

        public byte[] getIv() {
            return iv;
        }

        public byte[] getMac() {
            return mac;
        }

        /**
         * Construct a new bundle of ciphertext and IV.
         * @param c The ciphertext
         * @param i The IV
         * @param h The mac
         */
        public CipherTextIvMac(byte[] c, byte[] i, byte[] h) {
            cipherText = new byte[c.length];
            System.arraycopy(c, 0, cipherText, 0, c.length);
            iv = new byte[i.length];
            System.arraycopy(i, 0, iv, 0, i.length);
            mac = new byte[h.length];
            System.arraycopy(h, 0, mac, 0, h.length);
        }

        /**
         * Constructs a new bundle of ciphertext and IV from a string of the
         * format <code>base64(iv):base64(ciphertext)</code>.
         *
         * @param base64IvAndCiphertext A string of the format
         *            <code>iv:ciphertext</code> The IV and ciphertext must each
         *            be base64-encoded.
         */
        public CipherTextIvMac(String base64IvAndCiphertext) {
            String[] civArray = base64IvAndCiphertext.split(":");
            if (civArray.length != 3) {
                throw new IllegalArgumentException("Cannot parse iv:ciphertext:mac");
            } else {
                iv = Base64.decode(civArray[0], BASE64_FLAGS);
		Log.i("starwisp","inner iv: "+civArray[0]);
		Log.i("starwisp","inner iv hex: "+bytesToHex(iv));
                mac = Base64.decode(civArray[1], BASE64_FLAGS);
		Log.i("starwisp","inner mac: "+civArray[1]);
		Log.i("starwisp","inner mac hex: "+bytesToHex(mac));
                cipherText = Base64.decode(civArray[2], BASE64_FLAGS);
            }
        }

        /**
         * Concatinate the IV to the cipherText using array copy.
         * This is used e.g. before computing mac.
         * @param iv The IV to prepend
         * @param cipherText the cipherText to append
         * @return iv:cipherText, a new byte array.
         */
        public static byte[] ivCipherConcat(byte[] iv, byte[] cipherText) {
            byte[] combined = new byte[iv.length + cipherText.length];
            System.arraycopy(iv, 0, combined, 0, iv.length);
            System.arraycopy(cipherText, 0, combined, iv.length, cipherText.length);
            return combined;
        }

        /**
         * Encodes this ciphertext, IV, mac as a string.
         *
         * @return base64(iv) : base64(mac) : base64(ciphertext).
         * The iv and mac go first because they're fixed length.
         */
        @Override
        public String toString() {
            String ivString = Base64.encodeToString(iv, BASE64_FLAGS);
            String cipherTextString = Base64.encodeToString(cipherText, BASE64_FLAGS);
            String macString = Base64.encodeToString(mac, BASE64_FLAGS);
            return String.format(ivString + ":" + macString + ":" + cipherTextString);
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + Arrays.hashCode(cipherText);
            result = prime * result + Arrays.hashCode(iv);
            result = prime * result + Arrays.hashCode(mac);
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            CipherTextIvMac other = (CipherTextIvMac) obj;
            if (!Arrays.equals(cipherText, other.cipherText))
                return false;
            if (!Arrays.equals(iv, other.iv))
                return false;
            if (!Arrays.equals(mac, other.mac))
                return false;
            return true;
        }
    }

    /**
     * Copy the elements from the start to the end
     *
     * @param from  the source
     * @param start the start index to copy
     * @param end   the end index to finish
     * @return the new buffer
     */
    private static byte[] copyOfRange(byte[] from, int start, int end) {
        int length = end - start;
        byte[] result = new byte[length];
        System.arraycopy(from, start, result, 0, length);
        return result;
    }

    /**
     * Fixes for the RNG as per
     * http://android-developers.blogspot.com/2013/08/some-securerandom-thoughts.html
     *
     * This software is provided 'as-is', without any express or implied
     * warranty. In no event will Google be held liable for any damages arising
     * from the use of this software.
     *
     * Permission is granted to anyone to use this software for any purpose,
     * including commercial applications, and to alter it and redistribute it
     * freely, as long as the origin is not misrepresented.
     *
     * Fixes for the output of the default PRNG having low entropy.
     *
     * The fixes need to be applied via {@link #apply()} before any use of Java
     * Cryptography Architecture primitives. A good place to invoke them is in
     * the application's {@code onCreate}.
     */
    public static final class PrngFixes {

        private static final int VERSION_CODE_JELLY_BEAN = 16;
        private static final int VERSION_CODE_JELLY_BEAN_MR2 = 18;
        private static final byte[] BUILD_FINGERPRINT_AND_DEVICE_SERIAL = getBuildFingerprintAndDeviceSerial();

        /** Hidden constructor to prevent instantiation. */
        private PrngFixes() {
        }

        /**
         * Applies all fixes.
         *
         * @throws SecurityException if a fix is needed but could not be
         *             applied.
         */
        public static void apply() {
            applyOpenSSLFix();
            installLinuxPRNGSecureRandom();
        }

        /**
         * Applies the fix for OpenSSL PRNG having low entropy. Does nothing if
         * the fix is not needed.
         *
         * @throws SecurityException if the fix is needed but could not be
         *             applied.
         */
        private static void applyOpenSSLFix() throws SecurityException {
            if ((Build.VERSION.SDK_INT < VERSION_CODE_JELLY_BEAN)
                    || (Build.VERSION.SDK_INT > VERSION_CODE_JELLY_BEAN_MR2)) {
                // No need to apply the fix
                return;
            }

            try {
                // Mix in the device- and invocation-specific seed.
                Class.forName("org.apache.harmony.xnet.provider.jsse.NativeCrypto")
                        .getMethod("RAND_seed", byte[].class).invoke(null, generateSeed());

                // Mix output of Linux PRNG into OpenSSL's PRNG
                int bytesRead = (Integer) Class
                        .forName("org.apache.harmony.xnet.provider.jsse.NativeCrypto")
                        .getMethod("RAND_load_file", String.class, long.class)
                        .invoke(null, "/dev/urandom", 1024);
                if (bytesRead != 1024) {
                    throw new IOException("Unexpected number of bytes read from Linux PRNG: "
                            + bytesRead);
                }
            } catch (Exception e) {
                if (ALLOW_BROKEN_PRNG) {
                    Log.w(PrngFixes.class.getSimpleName(), "Failed to seed OpenSSL PRNG", e);
                } else {
                    throw new SecurityException("Failed to seed OpenSSL PRNG", e);
                }
            }
        }

        /**
         * Installs a Linux PRNG-backed {@code SecureRandom} implementation as
         * the default. Does nothing if the implementation is already the
         * default or if there is not need to install the implementation.
         *
         * @throws SecurityException if the fix is needed but could not be
         *             applied.
         */
        private static void installLinuxPRNGSecureRandom() throws SecurityException {
            if (Build.VERSION.SDK_INT > VERSION_CODE_JELLY_BEAN_MR2) {
                // No need to apply the fix
                return;
            }

            // Install a Linux PRNG-based SecureRandom implementation as the
            // default, if not yet installed.
            Provider[] secureRandomProviders = Security.getProviders("SecureRandom.SHA1PRNG");

            // Insert and check the provider atomically.
            // The official Android Java libraries use synchronized methods for
            // insertProviderAt, etc., so synchronizing on the class should
            // make things more stable, and prevent race conditions with other
            // versions of this code.
            synchronized (java.security.Security.class) {
                if ((secureRandomProviders == null)
                        || (secureRandomProviders.length < 1)
                        || (!secureRandomProviders[0].getClass().getSimpleName().equals("LinuxPRNGSecureRandomProvider"))) {
                    Security.insertProviderAt(new LinuxPRNGSecureRandomProvider(), 1);
                }

                // Assert that new SecureRandom() and
                // SecureRandom.getInstance("SHA1PRNG") return a SecureRandom backed
                // by the Linux PRNG-based SecureRandom implementation.
                SecureRandom rng1 = new SecureRandom();
                if (!rng1.getProvider().getClass().getSimpleName().equals("LinuxPRNGSecureRandomProvider")) {
                    if (ALLOW_BROKEN_PRNG) {
                        Log.w(PrngFixes.class.getSimpleName(),
                                "new SecureRandom() backed by wrong Provider: " + rng1.getProvider().getClass());
                        return;
                    } else {
                        throw new SecurityException("new SecureRandom() backed by wrong Provider: "
                                + rng1.getProvider().getClass());
                    }
                }

                SecureRandom rng2 = null;
                try {
                    rng2 = SecureRandom.getInstance("SHA1PRNG");
                } catch (NoSuchAlgorithmException e) {
                    if (ALLOW_BROKEN_PRNG) {
                        Log.w(PrngFixes.class.getSimpleName(), "SHA1PRNG not available", e);
                        return;
                    } else {
                        new SecurityException("SHA1PRNG not available", e);
                    }
                }
                if (!rng2.getProvider().getClass().getSimpleName().equals("LinuxPRNGSecureRandomProvider")) {
                    if (ALLOW_BROKEN_PRNG) {
                        Log.w(PrngFixes.class.getSimpleName(),
                                "SecureRandom.getInstance(\"SHA1PRNG\") backed by wrong" + " Provider: "
                                        + rng2.getProvider().getClass());
                        return;
                    } else {
                        throw new SecurityException(
                                "SecureRandom.getInstance(\"SHA1PRNG\") backed by wrong" + " Provider: "
                                        + rng2.getProvider().getClass());
                    }
                }
            }
        }

        /**
         * {@code Provider} of {@code SecureRandom} engines which pass through
         * all requests to the Linux PRNG.
         */
        private static class LinuxPRNGSecureRandomProvider extends Provider {

            public LinuxPRNGSecureRandomProvider() {
                super("LinuxPRNG", 1.0, "A Linux-specific random number provider that uses"
                        + " /dev/urandom");
                // Although /dev/urandom is not a SHA-1 PRNG, some apps
                // explicitly request a SHA1PRNG SecureRandom and we thus need
                // to prevent them from getting the default implementation whose
                // output may have low entropy.
                put("SecureRandom.SHA1PRNG", LinuxPRNGSecureRandom.class.getName());
                put("SecureRandom.SHA1PRNG ImplementedIn", "Software");
            }
        }

        /**
         * {@link SecureRandomSpi} which passes all requests to the Linux PRNG (
         * {@code /dev/urandom}).
         */
        public static class LinuxPRNGSecureRandom extends SecureRandomSpi {

            /*
             * IMPLEMENTATION NOTE: Requests to generate bytes and to mix in a
             * seed are passed through to the Linux PRNG (/dev/urandom).
             * Instances of this class seed themselves by mixing in the current
             * time, PID, UID, build fingerprint, and hardware serial number
             * (where available) into Linux PRNG.
             *
             * Concurrency: Read requests to the underlying Linux PRNG are
             * serialized (on sLock) to ensure that multiple threads do not get
             * duplicated PRNG output.
             */

            private static final File URANDOM_FILE = new File("/dev/urandom");

            private static final Object sLock = new Object();

            /**
             * Input stream for reading from Linux PRNG or {@code null} if not
             * yet opened.
             *
             * @GuardedBy("sLock")
             */
            private static DataInputStream sUrandomIn;

            /**
             * Output stream for writing to Linux PRNG or {@code null} if not
             * yet opened.
             *
             * @GuardedBy("sLock")
             */
            private static OutputStream sUrandomOut;

            /**
             * Whether this engine instance has been seeded. This is needed
             * because each instance needs to seed itself if the client does not
             * explicitly seed it.
             */
            private boolean mSeeded;

            @Override
            protected void engineSetSeed(byte[] bytes) {
                try {
                    OutputStream out;
                    synchronized (sLock) {
                        out = getUrandomOutputStream();
                    }
                    out.write(bytes);
                    out.flush();
                } catch (IOException e) {
                    // On a small fraction of devices /dev/urandom is not
                    // writable Log and ignore.
                    Log.w(PrngFixes.class.getSimpleName(), "Failed to mix seed into "
                            + URANDOM_FILE);
                } finally {
                    mSeeded = true;
                }
            }

            @Override
            protected void engineNextBytes(byte[] bytes) {
                if (!mSeeded) {
                    // Mix in the device- and invocation-specific seed.
                    engineSetSeed(generateSeed());
                }

                try {
                    DataInputStream in;
                    synchronized (sLock) {
                        in = getUrandomInputStream();
                    }
                    synchronized (in) {
                        in.readFully(bytes);
                    }
                } catch (IOException e) {
                    throw new SecurityException("Failed to read from " + URANDOM_FILE, e);
                }
            }

            @Override
            protected byte[] engineGenerateSeed(int size) {
                byte[] seed = new byte[size];
                engineNextBytes(seed);
                return seed;
            }

            private DataInputStream getUrandomInputStream() {
                synchronized (sLock) {
                    if (sUrandomIn == null) {
                        // NOTE: Consider inserting a BufferedInputStream
                        // between DataInputStream and FileInputStream if you need
                        // higher PRNG output performance and can live with future PRNG
                        // output being pulled into this process prematurely.
                        try {
                            sUrandomIn = new DataInputStream(new FileInputStream(URANDOM_FILE));
                        } catch (IOException e) {
                            throw new SecurityException("Failed to open " + URANDOM_FILE
                                    + " for reading", e);
                        }
                    }
                    return sUrandomIn;
                }
            }

            private OutputStream getUrandomOutputStream() throws IOException {
                synchronized (sLock) {
                    if (sUrandomOut == null) {
                        sUrandomOut = new FileOutputStream(URANDOM_FILE);
                    }
                    return sUrandomOut;
                }
            }
        }

        /**
         * Generates a device- and invocation-specific seed to be mixed into the
         * Linux PRNG.
         */
        private static byte[] generateSeed() {
            try {
                ByteArrayOutputStream seedBuffer = new ByteArrayOutputStream();
                DataOutputStream seedBufferOut = new DataOutputStream(seedBuffer);
                seedBufferOut.writeLong(System.currentTimeMillis());
                seedBufferOut.writeLong(System.nanoTime());
                seedBufferOut.writeInt(Process.myPid());
                seedBufferOut.writeInt(Process.myUid());
                seedBufferOut.write(BUILD_FINGERPRINT_AND_DEVICE_SERIAL);
                seedBufferOut.close();
                return seedBuffer.toByteArray();
            } catch (IOException e) {
                throw new SecurityException("Failed to generate seed", e);
            }
        }

        /**
         * Gets the hardware serial number of this device.
         *
         * @return serial number or {@code null} if not available.
         */
        private static String getDeviceSerialNumber() {
            // We're using the Reflection API because Build.SERIAL is only
            // available since API Level 9 (Gingerbread, Android 2.3).
            try {
                return (String) Build.class.getField("SERIAL").get(null);
            } catch (Exception ignored) {
                return null;
            }
        }

        private static byte[] getBuildFingerprintAndDeviceSerial() {
            StringBuilder result = new StringBuilder();
            String fingerprint = Build.FINGERPRINT;
            if (fingerprint != null) {
                result.append(fingerprint);
            }
            String serial = getDeviceSerialNumber();
            if (serial != null) {
                result.append(serial);
            }
            try {
                return result.toString().getBytes("UTF-8");
            } catch (UnsupportedEncodingException e) {
                throw new RuntimeException("UTF-8 encoding not supported");
            }
        }
    }
}
