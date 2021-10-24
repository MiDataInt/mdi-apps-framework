The code for this web page is available for anyone to run on 
their own computer. This might be a server in your laboratory
or your laptop - any modern computer will suffice.

**Why run the MDI myself?**

Here are a few reasons to run the MDI on your computer:

- you need a higher level of data security
- to help optimize resource management
- to engage in code development to improve the MDI

You will have complete control over the computer and the code
it is running. You can upload data files into your running
instance, re-write the code while the MDI is running, etc.

**How do I get started?**

The MDI is an R Shiny web server, so all you need is a
computer that can run R. The following link will take you to
the repository for the R package that will help you install
and run the MDI. The GitHub page has instructions - the
setup process is straightforward.

https://github.com/MiDataInt/mdi-manager

**Can I still use Globus data transfer when running locally?**

Yes, Globus is fully compatible with your local instance of the
MDI. You just need to do a bit of additional work to
activate your local instance for communication with Globus as
described below.

---

*The following steps are only needed if you plan
to use Globus to transfer files into the MDI Stage 2 apps.*

**Step 1 - Register a client application on Globus**

The MDI communicates with Globus as a "client application" that
you need to register on their web site, which will provide you
with credentials for your MDI installation.

Learn about the registration process here:

https://docs.globus.org/api/auth/developer-guide/#register-app

Then register your client application here:

https://developers.globus.org/

You must use this redirect URL (change the port if needed):

- http://localhost:3838/

and request the following scopes (the first three identify
the user and the last enables file transfers):

- openid
- email
- profile
- urn:globus:auth:scope:transfer.api.globus.org:all

You then need to copy the following two values in a secure location -
you'll need them below.

- your client id, i.e. key
- your secret (essentially a password, protect it!!)

**Step 2 - Create a Globus Connect Personal endpoint on your computer**

The MDI will transfer files from a server (e.g. Turbo at Michigan)
to a drive on the local computer where it is running - in Globus
parlance to an "endpoint" on your computer. You can easily create
that local endpoint by installing Globus Connect Personal, as described here:

https://www.globus.org/globus-connect-personal

When you are done, visit the following Globus page, find your new endpoint
and copy its "Endpoint UUID" from the Overview page.

https://app.globus.org/endpoints?scope=administered-by-me

**Step 3 - Provide your Globus client and endpoint values to the MDI**

Great! All that remains is to copy and paste your client id/key,
secret, and endpoint UUID values into the following file in your
installation of the MDI:

<code>/path/to/mdi/config.yml</code>

Start your web server by calling <code>mdi::run()</code>
in R and you should have access to Globus login and transfers. 
