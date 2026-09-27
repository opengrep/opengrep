require 'jwt'

# ok: ruby-jwt-hardcoded-secret
secret_const = 'secret-yo'

def bad1
    # ruleid: ruby-jwt-hardcoded-secret
    hmac_secret = 'my$ecretK3y'
    # ruleid: ruby-jwt-hardcoded-secret
    token = JWT.encode payload, hmac_secret, 'HS256'
    puts token
end

def bad2(token)
    # a method body does not see the file's local variables
    # ok: ruby-jwt-hardcoded-secret
    decoded_token = JWT.decode token, secret_const, true, { algorithm: 'HS256' }
    puts decoded_token
end

def bad3
    # ruleid: ruby-jwt-hardcoded-secret
    token = JWT.encode payload, 'hardcode', 'HS256'
    puts token
end

def bad4
    # ruleid: ruby-jwt-hardcoded-secret
    token = JWT.encode payload, nil, 'HS256'
    puts token
end

def ok1(secret_key)
    # ok: ruby-jwt-hardcoded-secret
    token = JWT.encode payload, hmac_secret, 'HS256'
    puts token
    decoded_token = JWT.decode token, secret_key, true, { algorithm: 'HS256' }
    puts decoded_token
end

def ok2()
    token = JWT.encode payload, hmac_secret, 'HS256'
    puts token
    jwk_loader = ->(options) do
        # jwk_loader implementation here
    end
    # ok: ruby-jwt-hardcoded-secret
    decoded_token = JWT.decode token, nil, true, { algorithm: 'HS256' }, jwks: jwk_loader
    puts decoded_token
end
