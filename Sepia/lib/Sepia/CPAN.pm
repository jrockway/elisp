package Sepia::CPAN;
use CPAN;
use LWP::Simple;

sub init
{
      CPAN::HandleConfig->load;
      CPAN::Shell::setup_output;
      CPAN::Index->reload;
}

sub list
{
    grep $_->inst_file, CPAN::Shell->expand('Module', shift || '/./');
}

sub interesting_parts
{
    my $mod = shift;
    +{ map { $_ => scalar $mod->$_ } qw(id cpan_version inst_version fullname cpan_file)};
}

sub outdated
{
    grep !$_->uptodate, list @_;
}

## stolen from CPAN::Shell...
sub readme
{
    my $dist = CPAN::Shell->expand('Module', shift);
    return unless $dist;
    $dist = $dist->cpan_file;
    # my ($dist) = $self->id;
    my ($sans, $suffix) = $dist =~ /(.+)\.(tgz|tar[\._-]gz|tar\.Z|zip)$/;
    my ($local_file);
    my ($local_wanted) = File::Spec->catfile(
        $CPAN::Config->{keep_source_where}, "authors", "id",
        split(/\//,"$sans.readme"));
    $local_file = CPAN::FTP->localize("authors/id/$sans.readme", $local_wanted);
    local (*IN, $/);
    open IN, $local_wanted;
    my $ret = <IN>;
    close IN;
    $ret;
}

sub perldoc
{
    get($CPAN::Defaultdocs . shift);
}

sub install
{
    my $dist = CPAN::Shell->expand('Module', shift);
    $dist->install if $dist;
}
