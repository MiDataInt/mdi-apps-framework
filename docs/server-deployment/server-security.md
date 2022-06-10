---
title: Server Security
parent: Server Deployment
has_children: false
nav_order: 10
---

## {{page.title}}

The MDI apps framework offers two ways of
authenticating and authorizing users to access 
the resources on public web servers: OAuth2
and access keys.

These mechanisms do not apply to web servers
running in remote or local modes. Such servers
are inherently secure because they cannot be
addressed on the internet and are only accessible
to the user on their own computer. Remote servers
are accessed via an SSH port tunnel and again
not publicly accessible.

The following definitions apply to the two related
but distinct concepts is server security:
- **authentication** - establishing who a user is
- **authorization** - determining what a user is allowed to do

### Authentication using OAuth2

The **OAuth2** protocol is an internet standard wherein
a user logs in with a third party identity provider who 
provides verifying information to the MDI server. Most people
will be familiar with this kind of login.

At present, the apps framework supports two identity providers:
[Globus](https://www.globus.org/)
and
[Google](https://www.google.com/).

The following example shows how to enable OAuth2 on your
public MDI server:

```yml
# mdi/config/stage2-apps.yml
access_control: oauth2
oauth2:
  host: google # or globus
  client:
    key:    abc # also called a client ID
    secret: xyz
```

For this to work, a developer running a public web server
must establish a static web domain to support
secure (SSL/TLS) connections as well as an account for that domain
at the desired OAuth2 provider. Detailed instructions can be
found at the vendor sites:

- [Globus](https://docs.globus.org/api/auth/)
- [Google](https://developers.google.com/identity/protocols/oauth2)

Once set up, simply copy your client ID and secret into _stage2-apps.yml_.

### Authorization using OAuth2

When using OAuth2 you assign access rights to individuals
by their validated email address. Groups of users
can be assigned the same rights, with wild-card matching
of email addresses as follows:

```yml
# mdi/config/stage2-apps.yml
user_groups:
  <groupName>: # replace with your group name
    emails:
      - "*@*"         # i.e., all authenticated email addresses
      - "*@umich.edu" # i.e., all users with a UMIch email   
      - wilmaflint@umich.edu
    paths: # see below
    apps:  # see below
```

The above example defines one group, but you can have as many
groups as needed. It shows the three ways you may specify email
addresses for matching, either for all users, by web domain, or
by individual email address (normally these would not be used
in the same group). Importantly, entries with wildcards (*)
must be quoted for proper YAML interpretation.

The assigned rights are set by the `paths` and `apps` keys, 
as described below.

### Authentication using access keys

As a moderate security alternative, you can also define 
**access keys** - essentially shared passwords - that you provide
to people you wish to give access to. Obviously, anyone who
learns an access key will also be able to use the site, but the 
mechanism nevertheless prevents most people from 
loading your MDI framework and apps and provides the opportunity
to authorize resources based on the access key.

The following example shows how to enable access keys on your
public MDI server:

```yml
# mdi/config/stage2-apps.yml
access_control: keys
```

### Authorization using access keys

When using access keys you assign rights per individual
access key as follows:

```yml
# mdi/config/stage2-apps.yml
keys:
  <keyName>: # replace with your key name
    hash: 5fr9uc24m0eqjbgxp1wnva6hd8okiltz_df6e4823c701569a0a2adb899f041e9d
    paths: # see below
    apps:  # see below
```

where _hash_ is an  _encrypted_ version of the access key 
that should be entered by a user. To convert your desired key to the
required hash, open R and call `mdi::password_store("myKey")`, 
installing the 
[mdi-manager R package](https://github.com/MiDataInt/mdi-manager)
first if necessary.

Do not worry about storing the hash openly in your config file.
The access key cannot be recovered from the salted hash, 
which is a one-way encryption. This includes you! If you forget
the key associated with a hash, you cannot recover it.

The assigned rights are set by the `paths` and `apps` keys, 
as described below.

### Assigning user rights

The first right, i.e, authorization, assigned to all
authenticated users is simply the ability to load the MDI
launch page.  In addition, you can specify the local drive
paths they can access and the apps they may use.

If you are using in-app local file access via
[shinyFiles](/mdi-apps-framework/docs/server-deployment/shinyFiles.html)
you first need
to name and list the allowed local paths in the **paths** key:

```yml
# mdi/config/stage2-apps.yml
paths:
  <pathName>: <server file path>
```

The authorization format is the same for OAuth2 and access keys, 
but recorded in their respective sections.

```yml
# mdi/config/stage2-apps.yml
user_groups:
  <groupName>: # replace with your group name
    paths: 
      read: all
      write: all
      load_default: <pathName>
    apps:  all
keys:
  <keyName>:
    paths: 
      read: 
        - <pathName>
      write: null
      load_default: <pathName>
    apps:  
      - <appName>
```

You can set allowed **paths** for reading and writing,
as well as the default path first shown to the user, where allowed values
are:
- **all** = user can read/write all entries in `paths`
- **null** = no read/write access allowed on any path
- a list of path names

For **apps**, allowed values are: 
- **all** = user can load all apps
- a list of the names of apps the user can load

### On-screen feedback on current user

The upper-right corner of the MDI app framework web page
always reports on the current user. In local and remote modes
this is the system user. In public server mode it is either 
the authenticated email address, in the case of OAuth2, or
the key group name, in the case of access keys.
