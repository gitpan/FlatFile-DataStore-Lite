#---------------------------------------------------------------------
  package FlatFile::DataStore::Lite::Preamble;
#---------------------------------------------------------------------

=head1 NAME

FlatFile::DataStore::Lite::Preamble - Perl module that implements a
lightweight flat file data store preamble class.

=head1 SYNOPSYS

 use FlatFile::DataStore::Lite::Preamble;

 my $preamble = FlatFile::DataStore::Lite::Preamble->new( {
     indicator => $indicator,  # single-character crud flag
     keynum    => $keynum,     # record sequence number (integer)
     reclen    => $reclen,     # record length (integer)
     thisfnum  => $fnum,       # file number (in base format)
     thisseek  => $datapos,    # seek position (integer)
     user      => $user_data,  # pre-formatted user-defined data
     } );

 my $string = $preamble->string(); # e.g.,

 # new preamble from existing preamble string
 # e.g., something like "=000I00Tw1001XNTest "

 my $clone = FlatFile::DataStore::Lite::Preamble->new( { string => $string } );

=head1 DESCRIPTION

FlatFile::DataStore::Lite::Preamble - Perl module that implements a
lightweight flat file data store preamble class.  This class defines
objects used by FlatFile::DataStore::Lite::Record and
FlatFile::DataStore::Lite.  So you will probably not ever call new()
yourself, but you might call some of the accessors either directly or
via a FF::DS::Record object;

A "preamble" is a string of fixed-length fields that precedes every
record in a FlatFile::DataStore::Lite data file.  In addition, this
string constitutes the entry in the data store key file for each
current record.

=head1 VERSION

FlatFile::DataStore::Lite::Preamble version 0.01

=cut

our $VERSION = '0.01';

use 5.008003;
use strict;
use warnings;

use Carp;
use Math::Int2Base qw( base_chars int2base base2int );
use Data::Omap qw( :ALL );

my %Generated = qw(
    string      1
    );

my %Attrs = ( %Generated, qw(
    indicator 1
    keynum    1
    reclen    1
    thisfnum  1
    thisseek  1
    user      1
    ) );

my $Ascii_chars = qr/^[ -~]+$/;

#---------------------------------------------------------------------

=head1 CLASS METHODS

=head2 FlatFile::DataStore::Lite::Preamble->new( $parms )

Constructs a new FlatFile::DataStore::Lite::Preamble object.

The parm C<$parms> is a hash reference containing key/value pairs to
populate the preamble string.  If there is a C<$parms->{'string'}>
value, it will be parsed into fields and the resulting key/value pairs
will replace the C<$parms> hash reference.

=cut

sub new {
    my( $class, $parms ) = @_;

    my $self = bless {}, $class;

    $self->init( $parms ) if $parms;
    return $self;
}

#---------------------------------------------------------------------
# init(), called by new() to parse the parms

sub init {
    my( $self, $parms ) = @_;

    my $datastore = $parms->{'datastore'} || croak "Missing datastore";
    if( my $string = $parms->{'string'} ) {
        $parms = $datastore->burst_preamble( $string );
    }

    my $crud   = $datastore->crud();
    my $create = $crud->{'create'};
    my $update = $crud->{'update'};
    my $delete = $crud->{'delete'};

    my $indicator = $parms->{'indicator'} || croak "Missing indicator";

    $self->indicator( $indicator );

    my $string = "";
    for my $href ( $datastore->specs() ) {  # each field is href of aref
        my( $field, $aref )     = %$href;
        my( $pos, $len, $parm ) = @$aref;
        my $value               = $parms->{ $field };

        for( $field ) {
            if( /indicator/ ) {
                croak qq'Missing value for "$_"' unless defined $value;
                croak qq'Invalid value for "$_" ($value)' unless length $value == $len;

                $self->{ $_ } = $value;
                $string      .= $value;
            }
            elsif( /user/ ) {
                croak qq'Missing value for "$_"' unless defined $value;
                croak qq'Invalid value for "$_" ($value)' unless $value =~ $Ascii_chars;

                my $try = sprintf "%-${len}s", $value;  # pads with blanks
                croak qq'Value of "$_" ($try) too long' if length $try > $len;

                $self->{ $_ } = $value;
                $string      .= $try;
            }
            elsif( not defined $value ) {
                if( (/keynum|reclen|thisfnum|thisseek/ ) ) {
                    croak qq'Missing value for "$_"';
                }
                $string .= "-" x $len;  # string of '-' for null
            }
            else {
                my $try = sprintf "%0${len}s", /fnum/? $value: int2base( $value, $parm );
                croak qq'Value of "$_" ($try) too long' if length $try > $len;

                $self->{ $_ } = /fnum/? $try: 0+$value;
                $string      .= $try;
            }
        }
    }

    croak qq'Something is wrong with preamble string: "$string"'
        unless $string =~ $datastore->regx();
    
    $self->string( $string );

    return $self;
}

#---------------------------------------------------------------------

=head1 OBJECT METHODS: ACCESSORS

The following methods set and return their respective attribute values
if C<$value> is given.  Otherwise, they just return the value.

 $preamble->string(    [$value] ); # full preamble string
 $preamble->indicator( [$value] ); # single-character crud indicator
 $preamble->keynum(    [$value] ); # record sequence number (integer)
 $preamble->reclen(    [$value] ); # record length (integer)
 $preamble->thisfnum(  [$value] ); # file number (in base format)
 $preamble->thisseek(  [$value] ); # seek position (integer)
 $preamble->user(      [$value] ); # pre-formatted user-defined data

Note: the class code uses these accessors to set values in the object
as it is assembling the preamble string in new().  Unless you have a
really good reason, you should not set these values yourself (outside
of a call to new()).  For example: setting the user data with user()
will I<not> change the user data in the C<string> attribute.

In other words, even though these are read/write accessors, you should
only use them for reading.

=cut

sub string    {for($_[0]->{string}    ){$_=$_[1]if@_>1;return$_}}
sub indicator {for($_[0]->{indicator} ){$_=$_[1]if@_>1;return$_}}
sub user      {for($_[0]->{user}      ){$_=$_[1]if@_>1;return$_}}

sub keynum    {for($_[0]->{keynum}    ){$_=0+$_[1]if@_>1;return$_}}
sub reclen    {for($_[0]->{reclen}    ){$_=0+$_[1]if@_>1;return$_}}
sub thisfnum  {for($_[0]->{thisfnum}  ){$_=  $_[1]if@_>1;return$_}}
sub thisseek  {for($_[0]->{thisseek}  ){$_=0+$_[1]if@_>1;return$_}}

__END__

=head1 AUTHOR

Brad Baxter, E<lt>bbaxter@cpan.orgE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2009 by Brad Baxter

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.8 or,
at your option, any later version of Perl 5 you may have available.

=cut

